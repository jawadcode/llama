use std::fmt::{self, Display};

use llamac_utils::{FmtItems, Ident, Spanned};

use crate::expr::{TypedCond, TypedIfThen, TypedMatch, TypedSpanExpr};

pub type TypedSpanStmt = Spanned<TypedStmt>;

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Const(TypedConst),
    LetBind(TypedLetBind),
    FunDef(TypedFunDef),
    IfThen(TypedIfThen),
    Cond(TypedCond),
    Match(TypedMatch),
}

impl Display for TypedStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypedStmt::Const(r#const) => r#const.fmt(f),
            TypedStmt::LetBind(let_bind) => let_bind.fmt(f),
            TypedStmt::FunDef(fun_def) => fun_def.fmt(f),
            TypedStmt::IfThen(if_then) => if_then.fmt(f),
            TypedStmt::Cond(cond) => cond.fmt(f),
            TypedStmt::Match(r#match) => r#match.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedConst {
    pub name: Spanned<Ident>,
    pub annot: Spanned<Type>,
    pub value: TypedSpanExpr,
}

impl Display for TypedConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {} : {} = {}", self.name, self.annot, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct TypedLetBind {
    pub name: Spanned<Ident>,
    pub annot: Option<Spanned<Type>>,
    pub value: TypedSpanExpr,
}

impl Display for TypedLetBind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("let ")?;
        self.name.fmt(f)?;
        if let Some(annot) = &self.annot {
            f.write_str(" : ")?;
            annot.fmt(f)?;
        }
        f.write_str(" = ")?;
        self.value.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunDef {
    pub name: Spanned<Ident>,
    pub params: Spanned<FunParams>,
    pub ret_ty: Spanned<Type>,
    pub body: TypedSpanExpr,
}

impl Display for TypedFunDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fun {}{} : {} = {}",
            self.name, self.params, self.ret_ty, self.body
        )
    }
}

#[derive(Debug, Clone)]
pub struct FunParams(pub Vec<Spanned<FunParam>>);

impl Display for FunParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", FmtItems::new(&self.0, ", "))
    }
}

#[derive(Debug, Clone)]
pub struct FunParam {
    pub name: Spanned<Ident>,
    pub annot: Spanned<Type>,
}

impl Display for FunParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.annot)
    }
}

#[derive(Debug, Clone)]
pub struct Types(pub Vec<Spanned<Type>>);

impl Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", FmtItems::new(&self.0, ", "))
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    /// Type constructors
    Fun {
        params: Spanned<Types>,
        ret_ty: Spanned<Box<Self>>,
    },
    List(Spanned<Box<Type>>),
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
