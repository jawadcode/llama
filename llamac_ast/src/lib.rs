use std::{
    fmt::{self, Display},
    path::PathBuf,
};

use llamac_utils::{FmtItems, Ident, Spanned};
use stmt::{Const, FunDef};

pub mod expr;
pub mod stmt;

#[derive(Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub items: Vec<Spanned<Item>>,
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(* {} *)\n\n{}",
            self.path.to_str().unwrap(),
            FmtItems::new(self.items.iter(), "\n")
        )
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
