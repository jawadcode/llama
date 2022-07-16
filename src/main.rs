use std::{env, fs, path::Path};

use llama::{lexer::Lexer, parser::Parser, typecheck::TypeChecker};

fn main() {
    let mut args = env::args();
    args.next();
    let path = args.next().unwrap();
    let path = Path::new(&path);
    let input = fs::read_to_string(path).unwrap();
    let tokens = Lexer::new(&input).collect::<Vec<_>>();
    println!(
        "Tokens: {}",
        tokens
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    );
    let mut parser = Parser::new(&input);
    match parser.parse_file(path) {
        Some(source_file) => {
            println!("AST: {source_file}");
            let mut engine = TypeChecker::new(source_file);
            engine.check().unwrap();
        }
        None => {
            eprintln!("Failed to parse");
        }
    }
}
