use std::fmt::{self, Display, Write};

use llamac_utils::{parens_fmt, FmtItems, Ident, Spanned};

use crate::{stmt::TypedSpanStmt, Type};

pub type TypedSpanExpr = Spanned<Box<TypedExpr>>;

#[derive(Debug, Clone)]
pub struct TypedExpr(InnerExpr, Type);

#[derive(Debug, Clone)]
pub enum InnerExpr {
    Ident(Ident),
    Literal(TypedLiteral),
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
pub enum TypedLiteral {
    Unit,
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl Display for TypedLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypedLiteral::Unit => f.write_str("unit"),
            TypedLiteral::String(string) => write!(f, "\"{string}\""),
            TypedLiteral::Int(int) => int.fmt(f),
            TypedLiteral::Float(float) => float.fmt(f),
            TypedLiteral::Bool(bool) => bool.fmt(f),
        }
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
pub enum UnOp {
    Not,
    Negate,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Not => f.write_char('!'),
            UnOp::Negate => f.write_char('-'),
        }
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
    pub args: Spanned<FunArgs>,
}

impl Display for TypedFunCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.fun, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct FunArgs(pub Vec<TypedSpanExpr>);

impl Display for FunArgs {
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
pub enum BinOp {
    // Arithmetic Operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Boolean Operators
    And,
    Or,
    Xor,
    // Relational Operators
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    // Misc
    Pipe,
    Concat,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => f.write_char('+'),
            BinOp::Sub => f.write_char('-'),
            BinOp::Mul => f.write_char('*'),
            BinOp::Div => f.write_char('/'),
            BinOp::Mod => f.write_char('%'),
            BinOp::And => f.write_str("and"),
            BinOp::Or => f.write_str("or"),
            BinOp::Xor => f.write_str("xor"),
            BinOp::Lt => f.write_char('<'),
            BinOp::Leq => f.write_str("<="),
            BinOp::Gt => f.write_char('>'),
            BinOp::Geq => f.write_str(">="),
            BinOp::Eq => f.write_str("=="),
            BinOp::Neq => f.write_str("!="),
            BinOp::Pipe => f.write_str("|>"),
            BinOp::Concat => f.write_str("++"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosure {
    pub params: Spanned<ClosureParams>,
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
pub struct ClosureParams(pub Vec<Spanned<ClosureParam>>);

impl Display for ClosureParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: Spanned<Ident>,
    pub annot: Option<Spanned<Type>>,
}

impl Display for ClosureParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(annot) = &self.annot {
            write!(f, "({} : {annot})", self.name)
        } else {
            self.name.fmt(f)
        }
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
    pub arms: Spanned<CondArms>,
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
pub struct CondArms(pub Vec<Spanned<CondArm>>);

impl Display for CondArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct CondArm {
    pub cond: TypedSpanExpr,
    pub branch: TypedSpanExpr,
}

impl Display for CondArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.cond, self.branch)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMatch {
    pub expr: TypedSpanExpr,
    pub arms: Spanned<MatchArms>,
}

impl Display for TypedMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match {} {} end", self.expr, self.arms)
    }
}

#[derive(Debug, Clone)]
pub struct MatchArms(pub Vec<Spanned<MatchArm>>);

impl Display for MatchArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub patterns: Spanned<MatchPatterns>,
    pub branch: TypedSpanExpr,
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.patterns, self.branch)
    }
}

#[derive(Debug, Clone)]
pub struct MatchPatterns(pub Vec<Spanned<MatchPattern>>);

impl Display for MatchPatterns {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ", ").fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    Wildcard,
    NamedWildcard(Ident),
    Literal(TypedLiteral),
}

impl Display for MatchPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MatchPattern::Wildcard => f.write_str("*"),
            MatchPattern::NamedWildcard(name) => name.fmt(f),
            MatchPattern::Literal(literal) => literal.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedBlock(pub Vec<TypedSpanExpr>);

impl Display for TypedBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "do {} end", FmtItems::new(&self.0, "; "))
    }
}
