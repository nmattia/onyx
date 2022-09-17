use crate::types;
use rnix::ast::{self, AstToken};
use rowan::ast::AstNode;

/* Expressions */

#[derive(Debug, Clone)]
pub enum Expr {
    StringLiteral(String),
    IntegerLiteral(i64),
    Lambda {
        param_id: String,
        param_ty: types::Type,
        body: Box<Expr>,
        quantifier: Option<String>,
    },
    Identifier(String),
    AttributeSet {
        attributes: Vec<(String, Expr)>,
    },
    Select {
        attrset: Box<Expr>,
        attrname: String,
    },
    Let {
        var_name: String,
        var_expr: Box<Expr>,
        body: Box<Expr>,
    },
    App {
        f: Box<Expr>,
        param: Box<Expr>,
    },
}

pub fn parse(s: &str) -> Result<Expr, String> {
    let ast = rnix::Root::parse(s).ok().map_err(|e| format!("{}", e))?;
    let expr = ast.expr().unwrap();
    to_expr(expr)
}

pub fn to_expr(expr: rnix::ast::Expr) -> Result<Expr, String> {
    let expr = match expr {
        rnix::ast::Expr::Literal(x) => to_expr_literal(x),
        rnix::ast::Expr::Str(s) => to_expr_str(s),
        rnix::ast::Expr::Ident(i) => Ok(to_expr_id(i)),
        rnix::ast::Expr::Lambda(l) => to_expr_lambda(l),
        rnix::ast::Expr::AttrSet(a) => to_expr_attrset(a),
        rnix::ast::Expr::Select(s) => to_expr_select(s),
        rnix::ast::Expr::LetIn(l) => to_expr_let(l),
        rnix::ast::Expr::Apply(a) => to_expr_app(a),
        rnix::ast::Expr::Paren(p) => to_expr_parens(p),
        _ => Err(format!("Onyx does not support expression: {:?}", expr)),
    };

    expr
}

fn to_expr_literal(x: rnix::ast::Literal) -> Result<Expr, String> {
    match x.kind() {
        ast::LiteralKind::Integer(f) => Ok(Expr::IntegerLiteral(f.value().unwrap())),
        _ => Err(format!("Onyx does not support literal: {:?}", x.kind())),
    }
}

fn to_expr_str(s: rnix::ast::Str) -> Result<Expr, String> {
    let mut parts: Vec<String> = vec![];

    for part in s.parts() {
        match part {
            ast::InterpolPart::Literal(l) => {
                let part: String = l.syntax().text().to_string();
                parts.push(part);
            }
            _ => return Err(format!("Onyx does not support str literal: {:?}", part)),
        }
    }

    Ok(Expr::StringLiteral(parts.join("")))
}

fn to_expr_id(i: rnix::ast::Ident) -> Expr {
    Expr::Identifier(i.ident_token().unwrap().text().to_string())
}

fn to_expr_lambda(s: rnix::ast::Lambda) -> Result<Expr, String> {
    let param = s.param().unwrap();

    let ty_annotation = comment_after(&param.syntax()).ok_or("missing annotation found")?;

    let (quantifier, ty_annotation) = types::parse::run_parser_leftover(
        &|s| types::parse::parse_try(s, &types::parse::parse_quantifier_prefix),
        &ty_annotation,
    )?;

    let ty = crate::types::parse::parse(ty_annotation.to_string())?;

    let param = match param {
        rnix::ast::Param::Pattern(_) => {
            return Err("Onyx does not support patterns in lambda".to_string())
        }
        rnix::ast::Param::IdentParam(p) => {
            p.ident().unwrap().ident_token().unwrap().text().to_string()
        }
    };

    let body = to_expr(s.body().unwrap())?;
    Ok(Expr::Lambda {
        param_id: param,
        param_ty: ty.clone(),
        quantifier,
        body: Box::new(body),
    })
}

fn to_expr_attrset(a: rnix::ast::AttrSet) -> Result<Expr, String> {
    use rnix::ast::HasEntry;

    let attributes: Vec<(String, Expr)> = a
        .entries()
        .map(|entry| match entry {
            rnix::ast::Entry::Inherit(_) => Err("inherit patterns are not implemented".to_string()),
            rnix::ast::Entry::AttrpathValue(av) => {
                let left = attrpath_str(av.attrpath().unwrap())?;
                let right = to_expr(av.value().unwrap())?;
                Ok((left, right))
            }
        })
        .collect::<Result<Vec<(String, Expr)>, String>>()?;

    Ok(Expr::AttributeSet { attributes })
}

fn to_expr_let(l: rnix::ast::LetIn) -> Result<Expr, String> {
    use rnix::ast::HasEntry;

    let assignment: Vec<rnix::ast::Entry> = l.entries().collect();
    if assignment.len() != 1 {
        return Err(format!("Onyx only supports a single assignment in let"));
    }

    let assignment = &assignment[0];

    let (var_name, var_expr) = match assignment {
        rnix::ast::Entry::Inherit(_) => {
            return Err("Onyx does not support 'inherit' patterns".to_string())
        }
        rnix::ast::Entry::AttrpathValue(av) => {
            let left = attrpath_str(av.attrpath().unwrap())?;
            let right = to_expr(av.value().unwrap())?;

            (left, right)
        }
    };

    let body = to_expr(l.body().unwrap())?;

    Ok(Expr::Let {
        var_name,
        var_expr: Box::new(var_expr),
        body: Box::new(body),
    })
}

fn attrpath_str(ap: rnix::ast::Attrpath) -> Result<String, String> {
    let attrs: Vec<rnix::ast::Attr> = ap.attrs().collect();
    if attrs.len() != 1 {
        return Err("Onyx only handles attrpaths of length 1".to_string());
    }

    let attrpath = &attrs[0];

    match attrpath {
        rnix::ast::Attr::Ident(i) => Ok(i.ident_token().unwrap().text().to_string()),
        _ => Err(format!("Unhandled attrpath: {:?}", attrpath)),
    }
}

fn to_expr_select(s: rnix::ast::Select) -> Result<Expr, String> {
    let expr = to_expr(s.expr().unwrap())?;
    let attrname = attrpath_str(s.attrpath().unwrap())?;

    Ok(Expr::Select {
        attrset: Box::new(expr),
        attrname: attrname.to_string(),
    })
}

fn to_expr_app(s: rnix::ast::Apply) -> Result<Expr, String> {
    let f = to_expr(s.lambda().unwrap())?;
    let param = to_expr(s.argument().unwrap())?;

    Ok(Expr::App {
        f: Box::new(f),
        param: Box::new(param),
    })
}

fn to_expr_parens(p: rnix::ast::Paren) -> Result<Expr, String> {
    to_expr(p.expr().unwrap())
}

/* rnix helpers */

use rnix;
use rnix::match_ast;

fn comment_after(node: &rnix::SyntaxNode) -> Option<String> {
    let foo = node
        .siblings_with_tokens(rowan::Direction::Next)
        // rowan always returns the first node for some reason
        .skip(1)
        .map_while(|element| match element {
            rnix::NodeOrToken::Token(token) => match_ast! {
                match token {
                    ast::Comment(it) => {
                        Some(Some(it))
                    },
                    ast::Whitespace(_) => Some(None),
                    _ => None,
                }
            },
            _ => None,
        })
        .find_map(|element| element)?;

    Some(foo.text().trim().to_string())
}
