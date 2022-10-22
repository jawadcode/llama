use std::fmt::{self, Display, Write};

use crate::{
    expr::{Cond, IfThen, Match, SpanExpr},
    utils::{FmtItems, Spanned},
    Ident,
};

pub type SpanStmt = Spanned<Stmt>;

#[derive(Clone)]
pub enum Stmt {
    Const(Const),
    LetBind(LetBind),
    FunDef(FunDef),
    IfThen(IfThen),
    Cond(Cond),
    Match(Match),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Const(r#const) => r#const.fmt(f),
            Stmt::LetBind(let_bind) => let_bind.fmt(f),
            Stmt::FunDef(fun_def) => fun_def.fmt(f),
            Stmt::IfThen(if_then) => if_then.fmt(f),
            Stmt::Cond(cond) => cond.fmt(f),
            Stmt::Match(r#match) => r#match.fmt(f),
        }
    }
}

#[derive(Clone)]
pub struct Const {
    pub name: Spanned<Ident>,
    pub annot: Spanned<TypeExpr>,
    pub value: SpanExpr,
}

impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("const ")?;
        self.name.fmt(f)?;
        f.write_str(" : ")?;
        self.annot.fmt(f)?;
        f.write_str(" = ")?;
        self.value.fmt(f)
    }
}

#[derive(Clone)]
pub struct LetBind {
    pub name: Spanned<Ident>,
    pub annot: Option<Spanned<TypeExpr>>,
    pub value: SpanExpr,
}

impl Display for LetBind {
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

#[derive(Clone)]
pub struct FunDef {
    pub name: Spanned<Ident>,
    pub params: Spanned<FunParams>,
    pub ret_ty: Spanned<TypeExpr>,
    pub body: SpanExpr,
}

impl Display for FunDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fun {}{} : {} = {}",
            self.name, self.params, self.ret_ty, self.body
        )
    }
}

#[derive(Clone)]
pub struct FunParams(pub Vec<Spanned<FunParam>>);

impl Display for FunParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        FmtItems::new(&self.0, ", ").fmt(f)?;
        f.write_char(')')
    }
}

#[derive(Clone)]
pub struct FunParam {
    pub name: Spanned<Ident>,
    pub annot: Spanned<TypeExpr>,
}

impl Display for FunParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        f.write_str(" : ")?;
        self.annot.fmt(f)
    }
}

#[derive(Clone)]
pub struct TypeExprs(pub Vec<Spanned<TypeExpr>>);

impl Display for TypeExprs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('[')?;
        FmtItems::new(&self.0, ", ").fmt(f)?;
        f.write_char(']')
    }
}

#[derive(Clone)]
pub enum TypeExpr {
    /// Type constructors
    Fun {
        params: Spanned<TypeExprs>,
        ret_ty: Spanned<Box<Self>>,
    },
    List(Spanned<Box<TypeExpr>>),
    // Primitives
    Unit,
    Bool,
    Int,
    Float,
    String,
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Fun { params, ret_ty } => {
                f.write_str("Fun")?;
                params.fmt(f)?;
                f.write_str(" -> ")?;
                ret_ty.fmt(f)
            }
            TypeExpr::List(ty) => {
                f.write_str("List[")?;
                ty.fmt(f)?;
                f.write_char(']')
            }
            TypeExpr::Unit => f.write_str("Unit"),
            TypeExpr::Bool => f.write_str("Bool"),
            TypeExpr::Int => f.write_str("Int"),
            TypeExpr::Float => f.write_str("Float"),
            TypeExpr::String => f.write_str("String"),
        }
    }
}
