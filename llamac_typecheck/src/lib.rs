use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use llamac_ast::{stmt::Type, Ident};

// The type inference engine
pub struct Engine {
    // The substitution from type variables to types to be built up
    subst: Vec<Type>,
    // A set of relationships between types
    constraints: Vec<Constraint>,
    // The context Γ
    context: Vec<HashMap<Ident, Type>>,
}

// A relationship between 2 types
#[derive(Debug, Clone)]
pub enum Constraint {
    Equality { type1: Type, type2: Type },
}

impl Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Equality { type1, type2 } => write!(f, "{type1} == {type2}"),
        }
    }
}
