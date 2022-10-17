use std::{fs, path::PathBuf};

use llamac_parser::Parser;

fn main() {
    let input = fs::read_to_string("test.txt").unwrap();
    match Parser::new(&input).parse_file(&PathBuf::from("test.txt")) {
        Ok(source_file) => println!("Pretty Printed AST:\n{source_file}"),
        Err(err) => err.report("test.txt".to_string(), &input),
    }
}
