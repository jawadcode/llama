use std::{env, fs, path::PathBuf};

use llamac_parser::Parser;

fn main() {
    let filename = env::args().nth(1).unwrap();
    let file = fs::read_to_string(&filename).unwrap();
    match Parser::new(&file).parse_file(&PathBuf::from("test.txt")) {
        Ok(source_file) => println!("Pretty Printed AST:\n{source_file}"),
        Err(err) => err.report(filename, &file),
    }
}
