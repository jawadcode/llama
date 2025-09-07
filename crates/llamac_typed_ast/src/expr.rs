use std::fmt::{self, Display, Write};

use llamac_ast::expr::{BinOp, Literal, MatchPatterns, UnOp};
use llamac_utils::{FmtItems, Ident, Spanned, parens_fmt};

use crate::{Type, stmt::TypedSpanStmt};

pub type TypedSpanExpr = Spanned<Box<TypedExpr>>;

#[derive(Debug, Clone)]
pub struct TypedExpr(pub InnerExpr, pub Type);

#[derive(Debug, Clone)]
pub enum InnerExpr {
    Ident(Ident),
    Literal(Literal),
    List(TypedList),
    UnaryOp(TypedUnaryOp),
    BinaryOp(TypedBinaryOp),
    FunCall(TypedFunCall),
    Closure(TypedClosure),
    IfThen(TypedIfThenExpr),
    Cond(TypedCondExpr),
    Match(TypedMatch),
    Block(TypedBlock),
    Stmt(TypedSpanStmt),
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        match &self.0 {
            InnerExpr::Ident(ident) => ident.fmt(f),
            InnerExpr::Literal(literal) => literal.fmt(f),
            InnerExpr::List(list) => list.fmt(f),
            InnerExpr::UnaryOp(unary_op) => parens_fmt!(f, unary_op),
            InnerExpr::BinaryOp(binary_op) => parens_fmt!(f, binary_op),
            InnerExpr::FunCall(fun_call) => parens_fmt!(f, fun_call),
            InnerExpr::Closure(closure) => parens_fmt!(f, closure),
            InnerExpr::IfThen(if_then) => if_then.fmt(f),
            InnerExpr::Cond(cond) => cond.fmt(f),
            InnerExpr::Match(r#match) => r#match.fmt(f),
            InnerExpr::Block(block) => block.fmt(f),
            InnerExpr::Stmt(stmt) => stmt.fmt(f),
        }?;
        write!(f, " : {})", self.1)
    }
}

#[derive(Debug, Clone)]
pub struct TypedList(pub Vec<TypedSpanExpr>);

impl Display for TypedList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", FmtItems::new(self.0.iter(), ", "))
    }
}

#[derive(Debug, Clone)]
pub struct TypedUnaryOp {
    pub op: Spanned<UnOp>,
    pub value: TypedSpanExpr,
}

impl Display for TypedUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.op, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct TypedBinaryOp {
    pub op: Spanned<BinOp>,
    pub lhs: TypedSpanExpr,
    pub rhs: TypedSpanExpr,
}

impl Display for TypedBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunCall {
    pub fun: TypedSpanExpr,
    pub args: Spanned<TypedFunArgs>,
}

impl Display for TypedFunCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.fun, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunArgs(pub Vec<TypedSpanExpr>);

impl Display for TypedFunArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(self.0.iter(), " ").fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosure {
    pub params: Spanned<TypedClosureParams>,
    pub ret_ty: Spanned<Type>,
    pub body: TypedSpanExpr,
}

impl Display for TypedClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("fn ")?;
        self.params.fmt(f)?;
        f.write_str(" : ")?;
        self.ret_ty.fmt(f)?;
        f.write_str(" => ")?;
        self.body.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosureParams(pub Vec<Spanned<TypedClosureParam>>);

impl Display for TypedClosureParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(self.0.iter(), ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosureParam {
    pub name: Spanned<Ident>,
    pub annot: Type,
}

impl Display for TypedClosureParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} : {})", self.name, self.annot)
    }
}

#[derive(Debug, Clone)]
pub struct TypedIfThenExpr {
    pub cond: TypedSpanExpr,
    pub then: TypedSpanExpr,
    pub r#else: TypedSpanExpr,
}

impl Display for TypedIfThenExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {} then {}", self.cond, self.then)?;
        write!(f, " else {}", self.r#else)
    }
}

#[derive(Debug, Clone)]
pub struct TypedCondExpr {
    pub arms: Spanned<TypedCondArms>,
    pub r#else: TypedSpanExpr,
}

impl Display for TypedCondExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cond {}", self.arms)?;
        write!(f, " | else => {}", self.r#else)?;
        f.write_str(" end")
    }
}

#[derive(Debug, Clone)]
pub struct TypedCondArms(pub Vec<Spanned<TypedCondArm>>);

impl Display for TypedCondArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(self.0.iter(), ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedCondArm {
    pub cond: TypedSpanExpr,
    pub target: TypedSpanExpr,
}

impl Display for TypedCondArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.cond, self.target)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatch {
    pub examinee: TypedSpanExpr,
    pub arms: Spanned<TypedMatchArms>,
}

impl Display for TypedMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match {} {} end", self.examinee, self.arms)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatchArms(pub Vec<Spanned<TypedMatchArm>>);

impl Display for TypedMatchArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(self.0.iter(), ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub patterns: Spanned<MatchPatterns>,
    pub target: TypedSpanExpr,
}

impl Display for TypedMatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.patterns, self.target)
    }
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub exprs: Vec<TypedSpanExpr>,
    pub tail: Option<TypedSpanExpr>,
}

impl Display for TypedBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "do {}; ", FmtItems::new(self.exprs.iter(), "; "))?;
        if let Some(tail) = &self.tail {
            write!(f, "{} end", tail)
        } else {
            write!(f, "end")
        }
    }
}
