use std::{convert::Infallible, env, path::PathBuf, process};

pub struct Config {
    pub input: PathBuf,
    pub output: OutLoc,
    pub only_scheme: bool,
}

pub enum OutLoc {
    Directory(PathBuf),
    File(PathBuf),
}

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const REPO: &str = env!("CARGO_PKG_REPOSITORY");
const LICENSE: &str = env!("CARGO_PKG_LICENSE");

pub fn parse_args(mut args: env::Args) -> Result<Config, String> {
    let exec_path = args.next().unwrap();
    let mut input = None;
    let mut output = OutLoc::Directory(
        env::current_dir()
            .expect("couldn't get current directory")
            .join("build"),
    );
    let mut only_scheme = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" => {
                help(&exec_path);
                process::exit(0)
            }
            "--version" => {
                version();
                process::exit(0)
            }
            "--only-scheme" | "-S" => only_scheme = true,
            "--output-dir" => {
                if let Some(dir) = args.next() {
                    output = OutLoc::Directory(PathBuf::from(dir));
                } else {
                    return Err("--output-dir requires an argument".to_string());
                }
            }
            "--output-file" => {
                if let Some(file) = args.next() {
                    output = OutLoc::File(PathBuf::from(file));
                } else {
                    return Err("--output-file requires an argument".to_string());
                }
            }
            "--output" | "-o" => {
                if let Some(out) = args.next() {
                    let out = PathBuf::from(out);
                    if out.is_file() {
                        output = OutLoc::File(out);
                    } else if out.is_dir() {
                        output = OutLoc::Directory(out);
                    } else {
                        return Err(
                            "path supplied to --output is not a file or directory".to_string()
                        );
                    }
                }
            }
            arg if arg.starts_with("-") => return Err(format!("unknown flag {arg}")),
            input_path => input = Some(PathBuf::from(input_path)),
        }
    }

    if let Some(input) = input {
        Ok(Config {
            input,
            output,
            only_scheme,
        })
    } else {
        Err("input file not provided".to_string())
    }
}

pub fn handle_error(error: String) -> Infallible {
    eprintln!("\x1b[1;31merror\x1b[0m: {error}");
    process::exit(1)
}

fn help(exe_path: &str) {
    println!("llamac {VERSION}, compiler for the Llama Programming Language.");
    println!("{AUTHORS}");
    println!("Repository: {REPO}\n");
    usage(exe_path);
}

fn version() {
    println!("llamac {VERSION}");
    println!("License {LICENSE}")
}

fn usage(exe_path: &str) {
    println!(
        "Usage: {exe_path} [--only-scheme|-S] (--output-dir <PATH>)|\n       (--output-file <PATH>)|(--output|--out <PATH>) input_file"
    )
}
