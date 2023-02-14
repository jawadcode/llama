use std::fmt::{self, Display, Write};

use llamac_utils::{parens_fmt, FmtItems, Spanned};

use crate::{
    stmt::{SpanStmt, Type},
    Ident,
};

pub type SpanExpr = Spanned<Box<Expr>>;

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    List(List),
    ListIndex(ListIndex),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FunCall(FunCall),
    Closure(Closure),
    IfThen(IfThen),
    Cond(Cond),
    Match(Match),
    Block(Block),
    Stmt(SpanStmt),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => ident.fmt(f),
            Expr::Literal(literal) => literal.fmt(f),
            Expr::List(list) => list.fmt(f),
            Expr::ListIndex(list_index) => list_index.fmt(f),
            Expr::UnaryOp(unary_op) => parens_fmt!(f, unary_op),
            Expr::BinaryOp(binary_op) => parens_fmt!(f, binary_op),
            Expr::FunCall(fun_call) => fun_call.fmt(f),
            Expr::Closure(closure) => parens_fmt!(f, closure),
            Expr::IfThen(if_then) => if_then.fmt(f),
            Expr::Cond(cond) => cond.fmt(f),
            Expr::Match(r#match) => r#match.fmt(f),
            Expr::Block(block) => block.fmt(f),
            Expr::Stmt(stmt) => parens_fmt!(f, stmt),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Unit,
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Unit => f.write_str("unit"),
            Literal::String(string) => write!(f, "\"{string}\""),
            Literal::Int(int) => int.fmt(f),
            Literal::Float(float) => float.fmt(f),
            Literal::Bool(bool) => bool.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct List(pub Vec<SpanExpr>);

impl Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", FmtItems::new(&self.0, ", "))
    }
}

#[derive(Debug, Clone)]
pub struct ListIndex {
    pub list: SpanExpr,
    pub index: SpanExpr,
}

impl Display for ListIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.list, self.index)
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub op: UnOp,
    pub value: SpanExpr,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.op, self.value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    INegate,
    FNegate,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Not => f.write_char('!'),
            UnOp::INegate => f.write_char('-'),
            UnOp::FNegate => f.write_str("-."),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub op: BinOp,
    pub lhs: SpanExpr,
    pub rhs: SpanExpr,
}

#[derive(Debug, Clone)]
pub struct FunCall {
    pub fun: SpanExpr,
    pub args: Spanned<FunArgs>,
}

impl Display for FunCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.fun, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct FunArgs(pub Vec<SpanExpr>);

impl Display for FunArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", FmtItems::new(&self.0, ", "))
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    // Arithmetic Operators
    Add,
    Sub,
    Mul,
    Div,
    FAdd,
    FSub,
    FMul,
    FDiv,
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
            BinOp::FAdd => f.write_str("+."),
            BinOp::FSub => f.write_str("-."),
            BinOp::FMul => f.write_str("*."),
            BinOp::FDiv => f.write_str("/."),
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
pub struct Closure {
    pub params: Spanned<ClosureParams>,
    pub ret_ty: Option<Spanned<Type>>,
    pub body: SpanExpr,
}

impl Display for Closure {
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
pub struct IfThen {
    pub cond: SpanExpr,
    pub then: SpanExpr,
    /// Is optional when used as a statement
    pub r#else: Option<SpanExpr>,
}

impl Display for IfThen {
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
pub struct Cond {
    pub arms: Spanned<CondArms>,
    pub r#else: Option<SpanExpr>,
}

impl Display for Cond {
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
    pub cond: SpanExpr,
    pub branch: SpanExpr,
}

impl Display for CondArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "| {} => {}", self.cond, self.branch)
    }
}

#[derive(Debug, Clone)]
pub struct Match {
    pub expr: SpanExpr,
    pub arms: Spanned<MatchArms>,
}

impl Display for Match {
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
    pub branch: SpanExpr,
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
    Literal(Literal),
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
pub struct Block(pub Vec<SpanExpr>);

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "do {} end", FmtItems::new(&self.0, "; "))
    }
}
