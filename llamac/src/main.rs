use std::{collections::HashMap, env, fs, path::PathBuf};

use llamac_parser::Parser;
use llamac_typecheck::Engine;
use llamac_typed_ast::{Type, Types};
use llamac_utils::{spanned, Ident};

fn main() {
    let filename = env::args().nth(1).unwrap();
    let file = fs::read_to_string(&filename).unwrap();
    match Parser::new(&file).parse_file(&PathBuf::from(&filename)) {
        Ok(source_file) => {
            println!("Pretty Printed AST:\n{:#?}", &source_file);
            /* let typed_source_file = Engine::new(
                [
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
                ]
                .into_iter()
                .collect::<HashMap<_, _>>(),
            )
            .infer_source_file(source_file)
            .unwrap();
            println!("Typed AST:\n{typed_source_file}");*/
        }
        Err(err) => err.report(filename, &file),
    }
}
