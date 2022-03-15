use std::fmt;

use derive_more::Display;

use crate::lexer::Span;

type SpanExpr = Spanned<Expr>;
type SpanStmt = Spanned<Stmt>;

type Boxpr = Box<SpanExpr>;
type Exprs = Vec<SpanExpr>;

/// Generic struct that allows you to attach a `Span` to `T`
#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "{}", node)]
pub struct Spanned<T>
where
    T: fmt::Debug + fmt::Display + Clone + PartialEq,
{
    pub span: Span,
    pub node: T,
}

#[derive(Debug, Display, Clone, PartialEq)]
/// The expression AST node
pub enum Expr {
    /// Literal
    #[display(fmt = "{}", _0)]
    Lit(Lit),
    /// Identifier
    #[display(fmt = "{}", _0)]
    Ident(String),
    /// If expression
    #[display(fmt = "(if :cond {} :then {} :else {})", cond, then, elss)]
    If {
        cond: Boxpr,
        then: Boxpr,
        elss: Boxpr,
    },
    /// Match/Switch expression
    #[display(fmt = "(match {} :arms [{}])", expr, "join(arms)")]
    Match { expr: Boxpr, arms: Vec<MatchArm> },
    /// Block expression
    #[display(fmt = "(block [{}])", "join(exprs)")]
    Block { exprs: Exprs },
    /// Anonymous function
    #[display(fmt = "(fn {})", _0)]
    Closure(Function),
    /// Statement as an expression in order to faciliate block expressions
    #[display(fmt = "{}", _0)]
    Stmt(SpanStmt),
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Stmt {
    /// Function definition
    #[display(fmt = "(fun {} {})", ident, fun)]
    FunDef { ident: String, fun: Function },
    /// Let binding
    #[display(fmt = "(let {} {})", ident, expr)]
    Let { ident: String, expr: Boxpr },
}

/// Literal
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Lit {
    /// String literal
    #[display(fmt = "\"{}\"", _0)]
    Str(String),
    /// Integer literal
    #[display(fmt = "{}", _0)]
    Int(i64),
    /// Float literal
    #[display(fmt = "{}", _0)]
    Float(f64),
    /// Boolean literal
    #[display(fmt = "{}", _0)]
    Bool(bool),
    /// Unit value literal
    #[display(fmt = "unit")]
    Unit,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "({} {})", pattern, expr)]
pub struct MatchArm {
    pub pattern: Lit,
    pub expr: SpanExpr,
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = ":params [{}] :body {}", "join(params)", body)]
pub struct Function {
    pub ident: Option<String>,
    pub params: Vec<String>,
    pub body: Boxpr,
}

fn join(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}
