use std::fmt::{self, Display};

pub mod expr;
pub mod stmt;
mod utils;

#[derive(Clone)]
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

pub struct SourceFile;

pub struct Item;
