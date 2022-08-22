
/* AST */

#[derive(Debug)]
pub enum Expr {
    StringLiteral(String),
}

fn parse(input: &str) -> Expr {
    to_expr(rnix::parse(input).node())
}

fn to_expr(ast: rnix::SyntaxNode) -> Expr {
    match ast.kind() {
        rnix::SyntaxKind::NODE_ROOT => to_expr(ast.first_child().unwrap()),
        rnix::SyntaxKind::NODE_STRING => {
            // TODO: this doesn't really read the string, i.e. it leaves surrounding quotes and
            // more
            Expr::StringLiteral(ast.text().to_string())
        },
        k => todo!("Not handled: {:?}", k),

    }
}


pub enum Type {
    Null,
    Boolean,
    Number,
    String,
    AttrSet { attrs: Vec<Attribute> },
}

pub struct Attribute { name: String, r#type: Type }


fn main() {
    println!("{:?}", parse("\"hi\""));
    println!("{:?}", parse("2"));
}

fn synth(expr: Expr) -> Type {
    match expr {
        Expr::StringLiteral(_) => Type::String,

    }
}




#[test]
fn foo() {
    println!("oh yeah");
}
