use std::{
    collections::HashMap,
    env,
    fs::{
        self,
        // File
    },
    // io::BufWriter,
    path::PathBuf,
    process,
};

use llamac_codegen_chez::compile_file;
use llamac_parser::Parser;
use llamac_typecheck::Engine as InferEngine;
use llamac_typed_ast::{Type, Types};
use llamac_utils::{Ident, spanned};

fn main() {
    let filename = env::args().nth(1).unwrap();
    let file = fs::read_to_string(&filename).unwrap();
    let source_file = Parser::new(&file)
        .parse_file(&PathBuf::from(&filename))
        .map_err(|err| {
            err.report(filename.clone(), &file);
            process::exit(1)
        })
        .unwrap();
    // println!("Pretty Printed AST:\n{}\n", &source_file);
    let init_ctx = [
        (
            Ident::new("print"),
            spanned! {
                0..0,
                Type::Fun {
                    params: Types(vec![Type::String]),
                    ret_ty: Box::new(Type::Unit)
                }
            },
        ),
        (
            Ident::new("print_int"),
            spanned! {
                0..0,
                Type::Fun {
                    params: Types(vec![Type::Int]),
                    ret_ty: Box::new(Type::Unit)
                }
            },
        ),
        (
            Ident::new("print_float"),
            spanned! {
                0..0,
                Type::Fun {
                    params: Types(vec![Type::Float]),
                    ret_ty: Box::new(Type::Unit)
                }
            },
        ),
    ];
    let mut engine = InferEngine::new(init_ctx.into_iter().collect::<HashMap<_, _>>());
    let typed_source_file = engine
        .infer_source_file(source_file)
        .and_then(|typed_source_file| engine.solve_constraints().map(|()| (typed_source_file)))
        .map_err(|err| {
            err.report(filename, &file);
            process::exit(1)
        })
        .unwrap();

    // println!("\nTyped AST (Unsolved):\n{}", typed_source_file.clone());
    // engine.dump_constraints();
    engine.solve_constraints().unwrap();
    // engine.dump_subst();
    let typed_source_file = engine.subst_source_file(typed_source_file);

    // let out_file = File::create("out.scm").unwrap();
    // let mut writer = BufWriter::new(out_file);
    compile_file(&mut std::io::stdout(), typed_source_file).unwrap();
    // println!("\nTyped AST (Solved):\n{typed_source_file}");
}
