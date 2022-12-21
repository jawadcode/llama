use std::fmt::{self, Display, Write};

use llamac_ast::expr::{BinOp, Literal, MatchPatterns, UnOp};
use llamac_utils::{parens_fmt, FmtItems, Ident, Spanned};

use crate::{stmt::TypedSpanStmt, Type};

pub type TypedSpanExpr = Spanned<Box<TypedExpr>>;

#[derive(Debug, Clone)]
pub struct TypedExpr(pub InnerExpr, pub Type);

#[derive(Debug, Clone)]
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
    Stmt(TypedSpanStmt),
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            InnerExpr::Ident(ident) => ident.fmt(f),
            InnerExpr::Literal(literal) => literal.fmt(f),
            InnerExpr::List(list) => list.fmt(f),
            InnerExpr::ListIndex(list_index) => list_index.fmt(f),
            InnerExpr::UnaryOp(unary_op) => parens_fmt!(f, unary_op),
            InnerExpr::BinaryOp(binary_op) => parens_fmt!(f, binary_op),
            InnerExpr::FunCall(fun_call) => fun_call.fmt(f),
            InnerExpr::Closure(closure) => parens_fmt!(f, closure),
            InnerExpr::IfThen(if_then) => if_then.fmt(f),
            InnerExpr::Cond(cond) => cond.fmt(f),
            InnerExpr::Match(r#match) => r#match.fmt(f),
            InnerExpr::Block(block) => block.fmt(f),
            InnerExpr::Stmt(stmt) => parens_fmt!(f, stmt),
        }?;
        write!(f, " : {}", self.1)
    }
}

#[derive(Debug, Clone)]
pub struct TypedList(pub Vec<TypedSpanExpr>);

impl Display for TypedList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", FmtItems::new(&self.0, ", "))
    }
}

#[derive(Debug, Clone)]
pub struct TypedListIndex {
    pub list: TypedSpanExpr,
    pub index: TypedSpanExpr,
}

impl Display for TypedListIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.list, self.index)
    }
}

#[derive(Debug, Clone)]
pub struct TypedUnaryOp {
    pub op: UnOp,
    pub value: TypedSpanExpr,
}

impl Display for TypedUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.op, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct TypedBinaryOp {
    pub op: BinOp,
    pub lhs: TypedSpanExpr,
    pub rhs: TypedSpanExpr,
}

#[derive(Debug, Clone)]
pub struct TypedFunCall {
    pub fun: TypedSpanExpr,
    pub args: Spanned<TypedFunArgs>,
}

impl Display for TypedFunCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.fun, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunArgs(pub Vec<TypedSpanExpr>);

impl Display for TypedFunArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", FmtItems::new(&self.0, ", "))
    }
}

impl Display for TypedBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosure {
    pub params: Spanned<TypedClosureParams>,
    pub ret_ty: Option<Spanned<Type>>,
    pub body: TypedSpanExpr,
}

impl Display for TypedClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("fn ")?;
        self.params.fmt(f)?;
        if let Some(ret_ty) = &self.ret_ty {
            f.write_str(" : ")?;
            ret_ty.fmt(f)?;
        }
        f.write_str(" => ")?;
        self.body.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosureParams(pub Vec<Spanned<TypedClosureParam>>);

impl Display for TypedClosureParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosureParam {
    pub name: Spanned<Ident>,
    pub annot: Spanned<Type>,
}

impl Display for TypedClosureParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} : {})", self.name, self.annot)
    }
}

#[derive(Debug, Clone)]
pub struct TypedIfThen {
    pub cond: TypedSpanExpr,
    pub then: TypedSpanExpr,
    /// Is optional when used as a statement
    pub r#else: Option<TypedSpanExpr>,
}

impl Display for TypedIfThen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {} then {}", self.cond, self.then)?;

        if let Some(r#else) = &self.r#else {
            write!(f, " else {else}")
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedCond {
    pub arms: Spanned<TypedCondArms>,
    pub r#else: Option<TypedSpanExpr>,
}

impl Display for TypedCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cond {}", self.arms)?;
        if let Some(r#else) = &self.r#else {
            write!(f, " | else => {else}")?;
        }
        f.write_str(" end")
    }
}

#[derive(Debug, Clone)]
pub struct TypedCondArms(pub Vec<Spanned<TypedCondArm>>);

impl Display for TypedCondArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedCondArm {
    pub cond: TypedSpanExpr,
    pub branch: TypedSpanExpr,
}

impl Display for TypedCondArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.cond, self.branch)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatch {
    pub expr: TypedSpanExpr,
    pub arms: Spanned<TypedMatchArms>,
}

impl Display for TypedMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match {} {} end", self.expr, self.arms)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatchArms(pub Vec<Spanned<TypedMatchArm>>);

impl Display for TypedMatchArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub patterns: Spanned<MatchPatterns>,
    pub branch: TypedSpanExpr,
}

impl Display for TypedMatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.patterns, self.branch)
    }
}

#[derive(Debug, Clone)]
pub struct TypedBlock(pub Vec<TypedSpanExpr>);

impl Display for TypedBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "do {} end", FmtItems::new(&self.0, "; "))
    }
}
