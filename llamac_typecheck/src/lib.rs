use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use llamac_typed_ast::Type;
use llamac_utils::Ident;

// The type inference engine
pub struct Engine {
    // The substitution from type variables to types to be built up
    subst: Vec<Type>,
    // A set of relationships between types
    constraints: Vec<Constraint>,
    // The context Γ
    context: Vec<HashMap<Ident, Type>>,
}

// A relationship between 2 `Types`s
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

impl Engine {
    pub fn new(env: HashMap<Ident, Type>) -> Self {
        Self {
            subst: Vec::new(),
            constraints: Vec::new(),
            context: vec![env],
        }
    }

    /// Push a new scope to the context
    fn enter_scope(&mut self) {
        self.context.push(HashMap::new());
    }

    // Pop the innermost scope from the context
    fn exit_scope(&mut self) {
        self.context.pop();
    }

    /// Get the type of an assumption from the context
    fn get_var(&self, name: &Ident) -> Option<Type> {
        for scope in self.context.iter().rev() {
            match scope.get(name) {
                Some(ty) => return Some(ty.clone()),
                None => continue,
            }
        }

        None
    }

    /// Extend the context with an assumption
    fn extend(&mut self, name: Ident, ty: Type) {
        self.context.last_mut().unwrap().insert(name, ty);
    }
}
