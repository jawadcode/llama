use std::fmt::{self, Display, Write};

use crate::{
    parens_fmt,
    stmt::SpanStmt,
    utils::{FmtItems, Spanned},
    Ident,
};

pub type SpanExpr = Spanned<Box<Expr>>;

#[derive(Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Range(Range),
    List(List),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    IfThen(IfThen),
    Cond(Cond),
    Block(Block),
    Stmt(SpanStmt),
    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => ident.fmt(f),
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Range(range) => parens_fmt!(f, range),
            Expr::List(list) => list.fmt(f),
            Expr::UnaryOp(unary_op) => parens_fmt!(f, unary_op),
            Expr::BinaryOp(binary_op) => parens_fmt!(f, binary_op),
            Expr::IfThen(if_then) => if_then.fmt(f),
            Expr::Cond(cond) => cond.fmt(f),
            Expr::Block(block) => block.fmt(f),
            Expr::Stmt(stmt) => parens_fmt!(f, stmt),
            Expr::Error => f.write_str("(* error node *)"),
        }
    }
}

#[derive(Clone)]
pub enum Literal {
    Unit,
    String(String),
    Number(f64),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Unit => f.write_str("unit"),
            Literal::String(string) => {
                f.write_char('"')?;
                string.fmt(f)?;
                f.write_char('"')
            }
            Literal::Number(number) => number.fmt(f),
            Literal::Bool(bool) => bool.fmt(f),
        }
    }
}

#[derive(Clone)]
pub struct Range {
    pub start: SpanExpr,
    pub end: SpanExpr,
}

impl Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.start.fmt(f)?;
        f.write_str("...")?;
        self.end.fmt(f)
    }
}

#[derive(Clone)]
pub struct List(Vec<SpanExpr>);

impl Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('[')?;
        FmtItems::new(&self.0, ", ").fmt(f)?;
        f.write_char(']')
    }
}

#[derive(Clone)]
pub struct UnaryOp {
    pub op: UnOp,
    pub value: SpanExpr,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.op.fmt(f)?;
        f.write_char(' ')?;
        self.value.fmt(f)
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct BinaryOp {
    pub op: BinOp,
    pub lhs: SpanExpr,
    pub rhs: SpanExpr,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct IfThen {
    pub cond: SpanExpr,
    pub then: SpanExpr,
    /// Is optional when used as a statement
    pub r#else: Option<SpanExpr>,
}

impl Display for IfThen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("if ")?;
        self.cond.fmt(f)?;
        f.write_str(" then ")?;
        self.then.fmt(f)?;

        if let Some(r#else) = &self.r#else {
            f.write_str(" else ")?;
            r#else.fmt(f)
        } else {
            Ok(())
        }
    }
}

#[derive(Clone)]
pub struct Cond {
    pub arms: Spanned<CondArms>,
    pub r#else: Option<SpanExpr>,
}

impl Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("cond ")?;
        self.arms.fmt(f)?;
        if let Some(r#else) = &self.r#else {
            f.write_str("| else => ")?;
            r#else.fmt(f)?;
        }
        f.write_str(" end")
    }
}

#[derive(Clone)]
pub struct CondArms(Vec<Spanned<CondArm>>);

impl Display for CondArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Clone)]
pub struct CondArm {
    pub cond: SpanExpr,
    pub branch: SpanExpr,
}

impl Display for CondArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("| ")?;
        self.cond.fmt(f)?;
        f.write_str(" => ")?;
        self.branch.fmt(f)
    }
}

#[derive(Clone)]
pub struct Match {
    pub expr: Expr,
    pub arms: Spanned<MatchArms>,
}

impl Display for Match {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("match ")?;
        self.expr.fmt(f)?;
        f.write_char(' ')?;
        self.arms.fmt(f)?;
        f.write_str(" end")
    }
}

#[derive(Clone)]
pub struct MatchArms(Vec<Spanned<MatchArm>>);

impl Display for MatchArms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ' ').fmt(f)
    }
}

#[derive(Clone)]
pub struct MatchArm {
    pub patterns: Spanned<MatchPatterns>,
    pub branch: SpanExpr,
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("| ")?;
        self.patterns.fmt(f)?;
        f.write_str(" => ")?;
        self.branch.fmt(f)
    }
}

#[derive(Clone)]
pub struct MatchPatterns(Vec<Spanned<MatchPattern>>);

impl Display for MatchPatterns {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtItems::new(&self.0, ", ").fmt(f)
    }
}

#[derive(Clone)]
pub enum MatchPattern {
    Wildcard,
    Literal(Literal),
    Range(Range),
    EmptyList,
    List(MatchPatterns),
    HeadTail(Spanned<Box<MatchPattern>>, Spanned<MatchPatterns>),
}

impl Display for MatchPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MatchPattern::Wildcard => f.write_str("*"),
            MatchPattern::Literal(literal) => literal.fmt(f),
            MatchPattern::Range(range) => range.fmt(f),
            MatchPattern::EmptyList => f.write_str("[]"),
            MatchPattern::List(list) => list.fmt(f),
            MatchPattern::HeadTail(head, tail) => {
                head.fmt(f)?;
                f.write_str(" ++ ")?;
                tail.fmt(f)
            }
        }
    }
}

#[derive(Clone)]
pub struct Block(Vec<SpanExpr>);

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("do ")?;
        FmtItems::new(&self.0, "; ").fmt(f)?;
        f.write_str(" end")
    }
}
