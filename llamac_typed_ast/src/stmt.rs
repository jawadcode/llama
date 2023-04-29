use std::fmt::{self, Display};

use llamac_utils::{FmtItems, Ident, Spanned};

use crate::{
    expr::{TypedCondArms, TypedMatch, TypedSpanExpr},
    Type,
};

pub type TypedSpanStmt = Spanned<TypedStmt>;

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Const(TypedConst),
    LetBind(TypedLetBind),
    FunDef(TypedFunDef),
    IfThen(TypedIfThenStmt),
    Cond(TypedCondStmt),
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
    pub params: Spanned<TypedFunParams>,
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
pub struct TypedFunParams(pub Vec<Spanned<TypedFunParam>>);

impl Display for TypedFunParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", FmtItems::new(self.0.iter(), ", "))
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunParam {
    pub name: Spanned<Ident>,
    pub annot: Spanned<Type>,
}

impl Display for TypedFunParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.annot)
    }
}

#[derive(Debug, Clone)]
pub struct TypedIfThenStmt {
    pub cond: TypedSpanExpr,
    pub then: TypedSpanExpr,
    pub r#else: Option<TypedSpanExpr>,
}

impl Display for TypedIfThenStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {} then {}", self.cond, self.then)?;
        if let Some(r#else) = &self.r#else {
            write!(f, " else {}", r#else)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedCondStmt {
    pub arms: Spanned<TypedCondArms>,
    pub r#else: Option<TypedSpanExpr>,
}

impl Display for TypedCondStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cond {}", self.arms)?;
        if let Some(r#else) = &self.r#else {
            write!(f, " | else => {}", r#else)?;
        }
        f.write_str(" end")
    }
}
