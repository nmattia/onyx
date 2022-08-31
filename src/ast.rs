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
        quantifier: Option<String>,
        body: Box<Expr>,
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

pub fn parse(s: &str) -> Expr {
    let ast = rnix::Root::parse(s).ok().unwrap();
    let expr = ast.expr().unwrap();
    to_expr(expr)
}

pub fn to_expr(expr: rnix::ast::Expr) -> Expr {
    let expr = match expr {
        rnix::ast::Expr::Literal(x) => to_expr_literal(x),
        rnix::ast::Expr::Str(s) => to_expr_str(s),
        rnix::ast::Expr::Ident(i) => to_expr_id(i),
        rnix::ast::Expr::Lambda(l) => to_expr_lambda(l),
        rnix::ast::Expr::AttrSet(a) => to_expr_attrset(a),
        rnix::ast::Expr::Select(s) => to_expr_select(s),
        rnix::ast::Expr::LetIn(l) => to_expr_let(l),
        rnix::ast::Expr::Apply(a) => to_expr_app(a),
        _ => todo!("expr not handled: {:?}", expr),
    };

    expr
}

fn to_expr_literal(x: rnix::ast::Literal) -> Expr {
    match x.kind() {
        ast::LiteralKind::Integer(f) => Expr::IntegerLiteral(f.value().unwrap()),
        _ => todo!(),
    }
}

fn to_expr_str(s: rnix::ast::Str) -> Expr {
    let mut parts: Vec<String> = vec![];

    for part in s.parts() {
        match part {
            ast::InterpolPart::Literal(l) => {
                let part: String = l.syntax().text().to_string();
                parts.push(part);
            }
            _ => todo!("Cannot handle non literal string"),
        }
    }

    Expr::StringLiteral(parts.join(""))
}

fn to_expr_id(i: rnix::ast::Ident) -> Expr {
    Expr::Identifier(i.ident_token().unwrap().text().to_string())
}

fn to_expr_lambda(s: rnix::ast::Lambda) -> Expr {
    let param = s.param().unwrap();

    let ty_str = comment_after(&param.syntax()).expect("missing type annotation");
    let ty = crate::types::parse::parse(ty_str);

    let param = match param {
        rnix::ast::Param::Pattern(_) => todo!("patterns are not supported in lambda"),
        rnix::ast::Param::IdentParam(p) => {
            p.ident().unwrap().ident_token().unwrap().text().to_string()
        }
    };

    let body = to_expr(s.body().unwrap());
    Expr::Lambda {
        param_id: param,
        param_ty: ty,
        quantifier: None,
        body: Box::new(body),
    }
}

fn to_expr_attrset(a: rnix::ast::AttrSet) -> Expr {
    use rnix::ast::HasEntry;

    let attributes = a
        .entries()
        .map(|entry| match entry {
            rnix::ast::Entry::Inherit(_) => todo!("inherit patterns are not implemented"),
            rnix::ast::Entry::AttrpathValue(av) => (
                attrpath_str(av.attrpath().unwrap()),
                to_expr(av.value().unwrap()),
            ),
        })
        .collect();

    Expr::AttributeSet { attributes }
}

fn to_expr_let(l: rnix::ast::LetIn) -> Expr {
    use rnix::ast::HasEntry;

    let assignment: Vec<rnix::ast::Entry> = l.entries().collect();
    if assignment.len() != 1 {
        panic!("let in must have exactly one assignment");
    }

    let assignment = &assignment[0];

    let (var_name, var_expr) = match assignment {
        rnix::ast::Entry::Inherit(_) => todo!("inherit patterns are not implemented"),
        rnix::ast::Entry::AttrpathValue(av) => (
            attrpath_str(av.attrpath().unwrap()),
            to_expr(av.value().unwrap()),
        ),
    };

    let body = to_expr(l.body().unwrap());

    Expr::Let {
        var_name,
        var_expr: Box::new(var_expr),
        body: Box::new(body),
    }
}

fn attrpath_str(ap: rnix::ast::Attrpath) -> String {
    let attrs: Vec<rnix::ast::Attr> = ap.attrs().collect();
    if attrs.len() != 1 {
        todo!("can only handle attrpaths of length 1");
    }

    let attrpath = &attrs[0];

    match attrpath {
        rnix::ast::Attr::Ident(i) => i.ident_token().unwrap().text().to_string(),
        _ => todo!("Unhandled attrpath: {:?}", attrpath),
    }
}

fn to_expr_select(s: rnix::ast::Select) -> Expr {
    let expr = to_expr(s.expr().unwrap());
    let attrname = attrpath_str(s.attrpath().unwrap());

    Expr::Select {
        attrset: Box::new(expr),
        attrname: attrname.to_string(),
    }
}

fn to_expr_app(s: rnix::ast::Apply) -> Expr {
    let f = to_expr(s.lambda().unwrap());
    let param = to_expr(s.argument().unwrap());

    Expr::App {
        f: Box::new(f),
        param: Box::new(param),
    }
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
