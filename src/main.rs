
use rnix::{
    ast::{self, AstToken},
};


#[derive(Debug)]
enum Expr {
    StringLiteral(String),
    IntegerLiteral(i64),
}

fn to_expr(s: &str) -> Expr {
    let ast = rnix::Root::parse(s).ok().unwrap();
    let expr = ast.expr().unwrap();
    match expr {
        ast::Expr::Literal(x) => {
            match x.kind() {
                ast::LiteralKind::Integer(f) => Expr::IntegerLiteral(f.value().unwrap()),
                _ => todo!(),

            }
        }
        ast::Expr::Str(s) => {

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
        },
        _ => todo!("expr not handled: {:?}", expr),
    }
}

fn main() {

    println!("{:?}", to_expr("2"));

}
