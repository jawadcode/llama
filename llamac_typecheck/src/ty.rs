use std::fmt::{self, Display};

use llamac_ast::{stmt::TypeExpr, utils::FmtItems};

#[derive(Clone)]
pub enum Type {
    /// Type constructors
    Fun {
        params: Vec<Self>,
        ret_ty: Box<Self>,
    },
    List(Box<Self>),
    // Primitives
    Unit,
    Bool,
    Int,
    Float,
    String,
    // Unknown types
    TypeVar(usize),
    Number,
    Comparable,
    Concatenable,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Fun { params, ret_ty } => {
                write!(f, "Fun[{}] -> {ret_ty}", FmtItems::new(&params, ", "))
            }
            Type::List(ty) => write!(f, "List[{ty}]"),
            Type::Unit => f.write_str("Unit"),
            Type::Bool => f.write_str("Bool"),
            Type::Int => f.write_str("Int"),
            Type::Float => f.write_str("Float"),
            Type::String => f.write_str("String"),
            Type::TypeVar(id) => write!(f, "$t{id}"),
            Type::Number => write!(f, "$Number"),
            Type::Comparable => write!(f, "$Comparable"),
            Type::Concatenable => write!(f, "$Concatenable"),
        }
    }
}

impl From<&TypeExpr> for Type {
    fn from(tyexpr: &TypeExpr) -> Self {
        match tyexpr {
            TypeExpr::Fun { params, ret_ty } => {
                let params: Vec<Self> = params
                    .node
                    .0
                    .iter()
                    .map(|param| (&param.node).into())
                    .collect();
                let ret_ty = Box::new(ret_ty.node.as_ref().into());
                Self::Fun { params, ret_ty }
            }
            TypeExpr::List(list) => Self::List(Box::new(list.node.as_ref().into())),
            TypeExpr::Unit => Type::Unit,
            TypeExpr::Bool => Type::Bool,
            TypeExpr::Int => Type::Int,
            TypeExpr::Float => Type::Float,
            TypeExpr::String => Type::String,
        }
    }
}
