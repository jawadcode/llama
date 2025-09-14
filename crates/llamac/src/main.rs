use std::{
    collections::HashMap,
    env::{self, consts::EXE_SUFFIX, current_dir},
    fs::{self, File, create_dir},
    // io::BufWriter,
    path::PathBuf,
    process::{self, Command},
};

use llamac_codegen_r6rs::compile_file;
use llamac_parser::Parser;
use llamac_typecheck::Engine as InferEngine;
use llamac_typed_ast::{Type, Types};
use llamac_utils::{Ident, spanned};

fn main() {
    let file_path = PathBuf::from(env::args().nth(1).unwrap());
    let file = fs::read_to_string(&file_path).unwrap();
    let source_file = Parser::new(&file)
        .parse_file(&PathBuf::from(&file_path))
        .map_err(|err| {
            err.report(file_path.to_string_lossy().into_owned(), &file);
            process::exit(1)
        })
        .unwrap();

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
            err.report(file_path.to_string_lossy().into_owned(), &file);
            process::exit(1)
        })
        .unwrap();

    engine.solve_constraints().unwrap();
    let typed_source_file = engine.subst_source_file(typed_source_file);

    let out_dir = current_dir().unwrap().join("build");
    let source_stem = file_path.file_stem().unwrap().to_string_lossy();
    let out_file_path = out_dir
        .clone()
        .join((source_stem.clone() + ".scm").as_ref());
    let exe_file_path = out_dir.clone().join((source_stem + EXE_SUFFIX).as_ref());

    if !out_dir.exists() {
        create_dir(out_dir).unwrap();
    }
    let mut out_file = File::create(&out_file_path).unwrap();
    compile_file(&mut out_file, typed_source_file).unwrap();
    Command::new("raco")
        .args([
            "exe",
            "--cs",
            "-o",
            exe_file_path.to_string_lossy().as_ref(),
            out_file_path.to_string_lossy().as_ref(),
        ])
        .spawn()
        .unwrap();
}
