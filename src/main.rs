use std::{env, fs, path::Path};

use llama::{parser::Parser, typecheck::TypeChecker};

fn main() {
    let mut args = env::args();
    args.next();
    let path = args.next().unwrap();
    let path = Path::new(&path);
    let input = fs::read_to_string(path).unwrap();

    let mut parser = Parser::new(&input);
    match parser.parse_file(path) {
        Some(source_file) => {
            let mut engine = TypeChecker::new(source_file);
            engine.check().unwrap();
        }
        None => {
            eprintln!("Failed to parse");
        }
    }
}
