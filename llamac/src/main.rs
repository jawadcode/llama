use std::{collections::HashMap, env, fs, path::PathBuf};

use llamac_parser::Parser;
use llamac_typecheck::Engine;

fn main() {
    let filename = env::args().nth(1).unwrap();
    let file = fs::read_to_string(&filename).unwrap();
    match Parser::new(&file).parse_file(&PathBuf::from("test.txt")) {
        Ok(source_file) => {
            println!("Pretty Printed AST:\n{}", &source_file);
            Engine::new(HashMap::new())
                .infer_source_file(source_file)
                .unwrap();
        }
        Err(err) => err.report(filename, &file),
    }
}
