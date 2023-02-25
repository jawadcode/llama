use std::{
    fmt::{self, Display},
    path::PathBuf,
};

use llamac_utils::{FmtItems, Spanned};
use stmt::{TypedConst, TypedFunDef};

pub mod expr;
pub mod stmt;

pub struct TypedSourceFile {
    pub path: PathBuf,
    pub items: Vec<Spanned<TypedItem>>,
}

impl Display for TypedSourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(* {} *)\n\n{}",
            self.path.to_str().unwrap(),
            FmtItems::new(&self.items, "\n")
        )
    }
}

#[derive(Debug, Clone)]
pub enum TypedItem {
    Const(TypedConst),
    FunDef(TypedFunDef),
}

impl Display for TypedItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypedItem::Const(r#const) => r#const.fmt(f),
            TypedItem::FunDef(fun_def) => fun_def.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Types(pub Vec<Type>);

impl Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", FmtItems::new(&self.0, ", "))
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Var(usize),
    /// Type constructors
    Fun {
        params: Types,
        ret_ty: Box<Self>,
    },
    List(Box<Type>),
    // Primitives
    Unit,
    Bool,
    Int,
    Float,
    String,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(id) => write!(f, "T{id}"),
            Type::Fun { params, ret_ty } => {
                write!(f, "Fun{params} -> {ret_ty}")
            }
            Type::List(ty) => {
                write!(f, "List[{ty}]")
            }
            Type::Unit => f.write_str("Unit"),
            Type::Bool => f.write_str("Bool"),
            Type::Int => f.write_str("Int"),
            Type::Float => f.write_str("Float"),
            Type::String => f.write_str("String"),
        }
    }
}

// Ideally we could implement this for `Spanned<Types>` but `Spanned` isn't defined in this crate so we can't
impl From<&llamac_ast::stmt::Types> for Types {
    fn from(value: &llamac_ast::stmt::Types) -> Self {
        Self(
            value
                .0
                .iter()
                .map(|Spanned { node, .. }| node.into())
                .collect(),
        )
    }
}

impl From<&llamac_ast::stmt::Type> for Type {
    fn from(value: &llamac_ast::stmt::Type) -> Self {
        use llamac_ast::stmt::Type as SType;
        match value {
            SType::Fun { params, ret_ty } => {
                let new_params: Vec<Type> = params
                    .node
                    .0
                    .iter()
                    .map(|Spanned { node, .. }| node.into())
                    .collect();
                Type::Fun {
                    params: Types(new_params),
                    ret_ty: Box::new(ret_ty.node.as_ref().into()),
                }
            }
            SType::List(ty) => Type::List(Box::new(ty.node.as_ref().into())),
            SType::Unit => Type::Unit,
            SType::Bool => Type::Bool,
            SType::Int => Type::Int,
            SType::Float => Type::Float,
            SType::String => Type::String,
        }
    }
}
