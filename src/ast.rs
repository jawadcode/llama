use std::{fmt, path::PathBuf};

use derive_more::Display;

use crate::{
    lexer::{Span, TK},
    utils::{join, join_sep},
};

pub type SpanExpr = Spanned<Expr>;
pub type SpanStmt = Spanned<Stmt>;

pub type Boxpr = Box<SpanExpr>;
pub type Exprs = Vec<SpanExpr>;

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
    /// Unary operator expression
    #[display(fmt = "({} {})", op, operand)]
    UnOp { op: TK, operand: Boxpr },
    /// Binary operator expression
    #[display(fmt = "({} {} {})", op, lhs, rhs)]
    BinOp { op: TK, lhs: Boxpr, rhs: Boxpr },
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
    /// Function call
    #[display(fmt = "(call {} :args [{}])", fun, "join(args)")]
    Call { fun: Boxpr, args: Exprs },
    /// Anonymous function
    #[display(fmt = "(fn :params [{}] :body {})", "join(params)", body)]
    Closure { params: Vec<String>, body: Boxpr },
    /// Statement as an expression in order to faciliate block expressions
    #[display(fmt = "{}", _0)]
    Stmt(SpanStmt),
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Stmt {
    /// Function definition
    #[display(
        fmt = "(fun {} :params [{}] :ret_type {} :body {})",
        ident,
        r#"params.iter().map(|(ident, ty)| format!("({ty} {ident})")).collect::<Vec<_>>().join(" ")"#,
        ret_type,
        body
    )]
    FunDef {
        ident: String,
        params: Vec<(String, TyIdent)>,
        ret_type: TyIdent,
        body: Boxpr,
    },
    /// Let binding
    #[display(
        fmt = "(let {} {})",
        r#"if let Some(annot) = annot { format!("({annot} {ident})") } else { ident.to_string() }"#,
        expr
    )]
    Let {
        ident: String,
        annot: Option<TyIdent>,
        expr: Boxpr,
    },
}

/// A source file
#[derive(Display)]
#[display(
    fmt = "PATH: {}\n\n{}",
    "path.to_string_lossy()",
    r#"join_sep(defs, "\n\n")"#
)]
pub struct SourceFile {
    pub path: PathBuf,
    pub defs: Vec<SpanStmt>,
}

/// Literal
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Lit {
    /// String literal
    #[display(fmt = r#""{}""#, _0)]
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

/// A match expression arm,
#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "([{}] {})", "join(patterns)", expr)]
pub struct MatchArm {
    pub patterns: Exprs,
    pub expr: SpanExpr,
}

/// A type identifier
#[derive(Debug, Display, Clone, PartialEq)]
pub enum TyIdent {
    #[display(fmt = "Number")]
    Num,
    #[display(fmt = "String")]
    Str,
    #[display(fmt = "Bool")]
    Bool,
    #[display(fmt = "Unit")]
    Unit,
    #[display(fmt = "Fun[{}] -> {}", "join(_0)", _1)]
    Fun(Vec<Self>, Box<Self>),
}
