use std::fmt::{self, Display, Write};

use llamac_ast::{
    parens_fmt,
    utils::{FmtItems, Spanned},
    Ident,
};

use crate::ty::Type;

use super::expr::{TypedCond, TypedExpr, TypedIfThen, TypedMatch};

#[derive(Clone)]
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
            TypedStmt::IfThen(if_then) => parens_fmt!(f, if_then),
            TypedStmt::Cond(cond) => parens_fmt!(f, cond),
            TypedStmt::Match(r#match) => parens_fmt!(f, r#match),
        }
    }
}

#[derive(Clone)]
pub struct TypedConst {
    pub name: Spanned<Ident>,
    pub annot: Spanned<Type>,
    pub value: TypedExpr,
}

impl Display for TypedConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {} : {} = {}", self.name, self.annot, self.value)
    }
}

#[derive(Clone)]
pub struct TypedLetBind {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Type>,
    pub value: TypedExpr,
}

impl Display for TypedLetBind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} : {} = {}", self.name, self.ty, self.value)
    }
}

#[derive(Clone)]
pub struct TypedFunDef {
    pub name: Spanned<Ident>,
    pub params: Spanned<TypedFunParams>,
    pub ret_ty: Spanned<Type>,
    pub body: TypedExpr,
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

#[derive(Clone)]
pub struct TypedFunParams(pub Vec<Spanned<TypedFunParam>>);

impl Display for TypedFunParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        parens_fmt!(f, FmtItems::new(&self.0, ", "))
    }
}

#[derive(Clone)]
pub struct TypedFunParam {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

impl Display for TypedFunParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.ty)
    }
}
