use std::{
    fmt::{self, Display},
    io::Write,
};

use llamac_ast::utils::Span;
use logos::{Logos, Skip, SpannedIter};

/// A wrapper around `logos::SpannedIter` which yields `Token::Eof` once it reaches the end of the source
pub struct Lexer<'source> {
    // Length of the source so `Token::Eof` can have the right span
    length: usize,
    logos: SpannedIter<'source, TK>,
    eof: bool,
}

impl<'source> Lexer<'source> {
    /// Returns a new `Lexer` which will operate on `source`
    pub fn new(source: &'source str) -> Self {
        Self {
            length: source.len(),
            logos: TK::lexer(source).spanned(),
            eof: false,
        }
    }
}

// When the iterator reaches the end of `self.source`, it will yield `Some(Token::Eof)` and then `None`
impl<'source> Iterator for Lexer<'source> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.logos.next() {
            Some((kind, span)) => Some(Token {
                kind,
                span: span.into(),
            }),
            None if self.eof => None,
            None => {
                self.eof = true;
                Some(Token {
                    kind: TK::Eof,
                    span: (self.length..self.length).into(),
                })
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TK,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Token {
    pub fn text<'source>(&self, source: &'source str) -> &'source str {
        &source[self.span]
    }
}

/// All of the token kinds
#[rustfmt::skip]
#[derive(Clone, Copy, Logos, PartialEq)]
pub enum TK {
    /* KEYWORDS */
    #[token("fun")]   Fun,
    #[token("const")] Const,
    #[token("let")]   Let,
    #[token("fn")]    Fn,
    #[token("if")]    If,
    #[token("then")]  Then,
    #[token("else")]  Else,
    #[token("cond")]  Cond,
    #[token("match")] Match,
    #[token("do")]    Do,
    #[token("end")]   End,
    
    /* LITERALS */
    #[regex(r"([A-Za-z]|_)([A-Za-z0-9]|_)*")] Ident,
    #[token("unit")]                          UnitLit,
    #[token("true")]                          True,
    #[token("false")]                         False,
    #[regex("[0-9]+", priority = 2)]          IntLit,
    #[regex(r#""([^"\\]|\\[\s\S])*""#)]       StringLit,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    FloatLit,

    /* OPERATORS */
    // ARITHMETIC OPERATORS
    #[token("+")] Add,
    #[token("-")] Sub,
    #[token("*")] Star,
    #[token("/")] Div,
    #[token("%")] Mod,
    // COMPARISON OPERATORS
    #[token("<")]  Lt,
    #[token("<=")] Leq,
    #[token(">")]  Gt,
    #[token(">=")] Geq,
    #[token("==")] Eq,
    #[token("!=")] Neq,
    // BOOLEAN OPERATORS
    #[token("!")]   Not,
    #[token("and")] And,
    #[token("or")]  Or,
    #[token("xor")] Xor,
    
    /* BRACKETS */
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LSquare,
    #[token("]")] RSquare,

    /* MISC */
    #[token("=")]  Assign,
    #[token("|>")] FnPipe,
    #[token("|")]  Pipe,
    #[token("++")] Concat,
    #[token(",")]  Comma,
    #[token("->")] Arrow,
    #[token("=>")] FatArrow,
    #[token(":")]  Colon,
    #[token(";")]  Semicolon,
    Eof,
        
    #[token("(*", comment_lexer)]
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[error]
    Error,
}

// Nested comments ftw
fn comment_lexer(lex: &mut logos::Lexer<TK>) -> Skip {
    let rem = lex.remainder();
    let mut nesting = 0;
    let mut previous = None;

    for (idx, current) in rem.char_indices() {
        if let Some(previous) = previous {
            match (previous, current) {
                ('(', '*') => nesting += 1,
                ('*', ')') if nesting > 0 => nesting -= 1,
                ('*', ')') => {
                    lex.bump(idx + 1);
                    return Skip;
                }
                _ => {}
            }
        }

        previous = Some(current);
    }

    Skip
}

impl Display for TK {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TK::Fun => f.write_str("'fun'"),
            TK::Const => f.write_str("'const'"),
            TK::Let => f.write_str("'let'"),
            TK::Fn => f.write_str("'fn'"),
            TK::If => f.write_str("'if'"),
            TK::Then => f.write_str("'then'"),
            TK::Else => f.write_str("'else'"),
            TK::Cond => f.write_str("'cond'"),
            TK::Match => f.write_str("'match'"),
            TK::Do => f.write_str("'do'"),
            TK::End => f.write_str("'end'"),
            TK::Ident => f.write_str("identifier"),
            TK::UnitLit => f.write_str("'unit'"),
            TK::True => f.write_str("'true'"),
            TK::False => f.write_str("'false'"),
            TK::IntLit => f.write_str("integer literal"),
            TK::StringLit => f.write_str("string literal"),
            TK::FloatLit => f.write_str("float literal"),
            TK::Add => f.write_str("'+'"),
            TK::Sub => f.write_str("'-'"),
            TK::Star => f.write_str("'*'"),
            TK::Div => f.write_str("'/'"),
            TK::Mod => f.write_str("'%'"),
            TK::Lt => f.write_str("'<'"),
            TK::Leq => f.write_str("'<='"),
            TK::Gt => f.write_str("'>'"),
            TK::Geq => f.write_str("'>='"),
            TK::Eq => f.write_str("'=='"),
            TK::Neq => f.write_str("'!='"),
            TK::Not => f.write_str("'!'"),
            TK::And => f.write_str("'and'"),
            TK::Or => f.write_str("'or'"),
            TK::Xor => f.write_str("'xor'"),
            TK::LParen => f.write_str("'('"),
            TK::RParen => f.write_str("')'"),
            TK::LSquare => f.write_str("'['"),
            TK::RSquare => f.write_str("']'"),
            TK::Assign => f.write_str("'='"),
            TK::FnPipe => f.write_str("'|>'"),
            TK::Pipe => f.write_str("'|'"),
            TK::Concat => f.write_str("'++'"),
            TK::Comma => f.write_str("','"),
            TK::Arrow => f.write_str("'->'"),
            TK::FatArrow => f.write_str("'=>'"),
            TK::Colon => f.write_str("':'"),
            TK::Semicolon => f.write_str("';'"),
            TK::Eof => f.write_str("EOF"),
            TK::Error => f.write_str("invalid token"),
        }
    }
}
