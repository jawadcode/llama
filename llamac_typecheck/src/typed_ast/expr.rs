use std::fmt::{self, Display, Write};

use llamac_ast::{
    expr::{BinOp, Literal, MatchPattern, UnOp},
    parens_fmt,
    utils::{FmtItems, Spanned},
    Ident,
};

use crate::ty::Type;

use super::stmt::TypedStmt;

#[derive(Clone)]
pub struct TypedExpr(pub Spanned<Box<InnerExpr>>, pub Type);

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.0, self.1)
    }
}

#[derive(Clone)]
pub enum InnerExpr {
    Ident(Ident),
    Literal(Literal),
    List(TypedList),
    ListIndex(TypedListIndex),
    UnaryOp(TypedUnaryOp),
    BinaryOp(TypedBinaryOp),
    FunCall(TypedFunCall),
    Closure(TypedClosure),
    IfThen(TypedIfThen),
    Cond(TypedCond),
    Match(TypedMatch),
    Block(TypedBlock),
    Stmt(TypedStmt),
}

impl Display for InnerExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InnerExpr::Ident(ident) => ident.fmt(f),
            InnerExpr::Literal(literal) => literal.fmt(f),
            InnerExpr::List(list) => list.fmt(f),
            InnerExpr::ListIndex(list_index) => list_index.fmt(f),
            InnerExpr::UnaryOp(unary_op) => parens_fmt!(f, unary_op),
            InnerExpr::BinaryOp(binary_op) => parens_fmt!(f, binary_op),
            InnerExpr::FunCall(fun_call) => fun_call.fmt(f),
            InnerExpr::Closure(closure) => parens_fmt!(f, closure),
            InnerExpr::IfThen(if_then) => parens_fmt!(f, if_then),
            InnerExpr::Cond(cond) => cond.fmt(f),
            InnerExpr::Match(r#match) => r#match.fmt(f),
            InnerExpr::Block(block) => block.fmt(f),
            InnerExpr::Stmt(stmt) => parens_fmt!(f, stmt),
        }
    }
}

#[derive(Clone)]
pub struct TypedList(pub Vec<TypedExpr>);

impl Display for TypedList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ", ").fmt(f)
    }
}

#[derive(Clone)]
pub struct TypedListIndex {
    pub list: TypedExpr,
    pub index: TypedExpr,
}

impl Display for TypedListIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.list, self.index)
    }
}

#[derive(Clone)]
pub struct TypedUnaryOp {
    pub op: UnOp,
    pub value: TypedExpr,
}

impl Display for TypedUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.op, self.value)
    }
}

#[derive(Clone)]
pub struct TypedBinaryOp {
    pub op: BinOp,
    pub lhs: TypedExpr,
    pub rhs: TypedExpr,
}

impl Display for TypedBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Clone)]
pub struct TypedFunArgs(pub Vec<TypedExpr>);

impl Display for TypedFunArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ", ").fmt(f)
    }
}

#[derive(Clone)]
pub struct TypedFunCall {
    pub fun: TypedExpr,
    pub args: Spanned<TypedFunArgs>,
}

impl Display for TypedFunCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.fun, self.args)
    }
}

#[derive(Clone)]
pub struct TypedClosure {
    pub params: Spanned<TypedClosureParams>,
    pub ret_ty: Spanned<Type>,
    pub body: TypedExpr,
}

impl Display for TypedClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {} : {} => {}", self.params, self.ret_ty, self.body)
    }
}

#[derive(Clone)]
pub struct TypedClosureParams(pub Vec<Spanned<TypedClosureParam>>);

impl Display for TypedClosureParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Clone)]
pub struct TypedClosureParam {
    pub name: Spanned<Ident>,
    pub ty: Type,
}

impl Display for TypedClosureParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} : {})", self.name, self.ty)
    }
}

#[derive(Clone)]
pub struct TypedIfThen {
    pub cond: TypedExpr,
    pub then: TypedExpr,
    pub r#else: Option<TypedExpr>,
}

impl Display for TypedIfThen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {} then {}", self.cond, self.then)?;
        if let Some(r#else) = &self.r#else {
            write!(f, " else {}", r#else)
        } else {
            Ok(())
        }
    }
}

#[derive(Clone)]
pub struct TypedCond {
    pub arms: Spanned<TypedCondArms>,
    pub r#else: Option<TypedExpr>,
}

impl Display for TypedCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cond {}", self.arms)?;
        if let Some(r#else) = &self.r#else {
            write!(f, " | else => {}", r#else)?;
        }
        f.write_str(" end")
    }
}

#[derive(Clone)]
pub struct TypedCondArms(pub Vec<Spanned<TypedCondArm>>);

impl Display for TypedCondArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Clone)]
pub struct TypedCondArm {
    pub cond: TypedExpr,
    pub branch: TypedExpr,
}

impl Display for TypedCondArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.cond, self.branch)
    }
}

#[derive(Clone)]
pub struct TypedMatch {
    pub expr: TypedExpr,
    pub arms: Spanned<TypedMatchArms>,
}

impl Display for TypedMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match {} {} end", self.expr, self.arms)
    }
}

#[derive(Clone)]
pub struct TypedMatchArms(pub Vec<Spanned<TypedMatchArm>>);

impl Display for TypedMatchArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Clone)]
pub struct TypedMatchArm {
    pub pattern: Spanned<TypedMatchPatterns>,
    pub branch: TypedExpr,
}

impl Display for TypedMatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.pattern, self.branch)
    }
}

#[derive(Clone)]
pub struct TypedMatchPatterns(pub Vec<Spanned<TypedMatchPattern>>);

impl Display for TypedMatchPatterns {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ", ").fmt(f)
    }
}

#[derive(Clone)]
pub struct TypedMatchPattern {
    pub pattern: MatchPattern,
    pub ty: Type,
}

impl Display for TypedMatchPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.pattern, self.ty)
    }
}

#[derive(Clone)]
pub struct TypedBlock(pub Vec<TypedExpr>);

impl Display for TypedBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "do {} end", FmtItems::new(&self.0, "; "))
    }
}
