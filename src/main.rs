mod ast;
mod typecheck;
mod types;

// TODO: review memory usage & remove clones

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    for arg in args {
        let ast = rnix::Root::parse(&arg).ok().unwrap();
        let expr = ast.expr().unwrap();
        let expr = ast::to_expr(expr);

        println!("AST: {:?}", expr);

        let synth = typecheck::synthesize(&expr);

        println!("Type: {}", synth);
    }
}
