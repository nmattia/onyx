use rnix::{
    ast::{self, AstToken},
};

/* Types */

#[derive(Debug)]
enum Type {
    Integer
}

fn parse_type_annotation(s: String) -> Type {
    match s.as_str() {
        "integer" => Type::Integer,
        _ => panic!("Dunno how to parse type annotation '{:?}'", s),
    }
}

/* Expressions */

#[derive(Debug)]
enum Expr {
    StringLiteral(String),
    IntegerLiteral(i64),
    Lambda{param: Box<Expr>, body: Box<Expr>},
    Identifier(String),


    Annotated{expr: Box<Expr>, ty: Type}
}


fn to_expr(expr: rnix::ast::Expr) -> Expr {
    let comment = comment_after(&expr.syntax());
    let expr = match expr {
        ast::Expr::Literal(x) => to_expr_literal(x),
        ast::Expr::Str(s) =>  to_expr_str(s) ,
        ast::Expr::Lambda(l) =>  to_expr_lambda(l) ,
        _ => todo!("expr not handled: {:?}", expr),
    };

    match comment {
        Some(comment) => Expr::Annotated{ expr: Box::new(expr), ty: parse_type_annotation(comment) },
        None => expr,
    }
}

fn to_expr_literal(x: rnix::ast::Literal) -> Expr {
    match x.kind() {
        ast::LiteralKind::Integer(f) => Expr::IntegerLiteral(f.value().unwrap()),
        _ => todo!(),
    }
}

fn to_expr_str(s: rnix::ast::Str) -> Expr{
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

fn to_expr_lambda(s: rnix::ast::Lambda) -> Expr {
    let param = match s.param().unwrap() {
        rnix::ast::Param::Pattern(_) => todo!("patterns are not supported in lambda"),
        rnix::ast::Param::IdentParam(p) => Expr::Identifier(p.ident().unwrap().ident_token().unwrap().text().to_string()),
    };

    let body = to_expr(s.body().unwrap());
    Expr::Lambda{param: Box::new(param), body: Box::new(body)}
}

use rowan::ast::AstNode;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    for arg in args {
        let ast = rnix::Root::parse(&arg).ok().unwrap();
        let expr = ast.expr().unwrap();

        println!("{:?}", to_expr(expr));
    }
}

/* rnix helpers */

use rnix::match_ast;

fn comment_after(node: &rnix::SyntaxNode) -> Option<String> {
    let foo = node.siblings_with_tokens(rowan::Direction::Next)
        // rowan always returns the first node for some reason
        .skip(1)
        .map_while(|element| match element {
            rnix::NodeOrToken::Token(token) => match_ast! {
                match token {
                    ast::Comment(it) => {
                        println!("{:?}", &it);
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
