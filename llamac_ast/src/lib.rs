use std::{
    fmt::{self, Display},
    path::PathBuf,
};

use stmt::{Const, FunDef};
use utils::{FmtItems, Spanned};

pub mod expr;
pub mod stmt;
pub mod utils;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(string: &str) -> Self {
        Self(string.to_string())
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ident) = self;
        f.write_str(ident)
    }
}

pub struct SourceFile {
    pub path: PathBuf,
    pub items: Vec<Spanned<Item>>,
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("(* ")?;
        f.write_str(self.path.to_str().unwrap())?;
        f.write_str(" *)\n\n")?;
        FmtItems::new(&self.items, "\n").fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Const(Const),
    FunDef(FunDef),
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Const(r#const) => r#const.fmt(f),
            Item::FunDef(fun_def) => fun_def.fmt(f),
        }
    }
}
