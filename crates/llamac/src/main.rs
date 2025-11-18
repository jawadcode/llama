mod cli;

use std::{
    collections::HashMap,
    env,
    fs::{self, File},
    path::PathBuf,
    process::{self, Command},
};

use llamac_codegen_r6rs::compile_file;
use llamac_parser::Parser;
use llamac_typecheck::Engine as InferEngine;
use llamac_typed_ast::{Type, Types};
use llamac_utils::{Ident, spanned};

use crate::cli::{OutLoc, handle_error, parse_args};

fn main() {
    let config = parse_args(env::args()).map_err(handle_error).unwrap();
    let file = fs::read_to_string(&config.input).unwrap();
    let source_file = Parser::new(&file)
        .parse_file(&PathBuf::from(&config.input))
        .map_err(|err| {
            err.report(config.input.to_string_lossy().into_owned(), &file);
            process::exit(1)
        })
        .unwrap();

    println!("AST:\n{source_file}");

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
        (
            Ident::new("print_list_ints"),
            spanned! {
                0..0,
                Type::Fun {
                    params: Types(vec![Type::List(Box::new(Type::Int))]),
                    ret_ty: Box::new(Type::Unit)
                }
            },
        ),
    ];

    let mut engine = InferEngine::new(init_ctx.into_iter().collect::<HashMap<_, _>>());
    let typed_source_file = engine
        .infer_source_file(source_file)
        .and_then(|typed_source_file| engine.solve_constraints().map(|()| typed_source_file))
        .map_err(|err| {
            err.report(config.input.to_string_lossy().into_owned(), &file);
            process::exit(1)
        })
        .unwrap();

    engine.solve_constraints().unwrap();
    let typed_source_file = engine.subst_source_file(typed_source_file);
    println!("\nTyped AST:\n{typed_source_file}");

    if config.only_scheme {
        let out_file_path = match config.output {
            OutLoc::File(file) => file,
            OutLoc::Directory(dir) => dir
                .join(config.input.file_stem().unwrap())
                .with_extension("scm"),
        };
        let mut out_file =
            File::create(out_file_path).expect("Output file or directory does not exist");
        compile_file(&mut out_file, typed_source_file).unwrap();
    } else {
        let (scm_file_path, exe_file_path) = match config.output {
            OutLoc::File(file) => (file.clone().with_extension("scm"), file),
            OutLoc::Directory(dir) => {
                let artifact = dir.join(config.input.file_stem().unwrap());
                (
                    artifact.with_extension("scm"),
                    artifact.with_extension(env::consts::EXE_EXTENSION),
                )
            }
        };
        let builddir = scm_file_path.parent().unwrap();
        if !builddir.exists() {
            fs::create_dir_all(builddir).unwrap();
        }
        let mut out_file = File::create(&scm_file_path).unwrap();
        compile_file(&mut out_file, typed_source_file).unwrap();
        Command::new("raco")
            .args([
                "exe",
                "--cs",
                "--embed-dlls",
                "-o",
                exe_file_path.to_string_lossy().as_ref(),
                scm_file_path.to_string_lossy().as_ref(),
            ])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
    }
}
