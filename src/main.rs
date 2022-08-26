mod ast;
mod typecheck;
mod types;

// TODO: review memory usage & remove clones

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    for arg in args {
        let expr = ast::parse(&arg);

        println!("AST: {:?}", expr);

        let synth = typecheck::synthesize(&expr);

        println!("Type: {}", synth);
    }
}
