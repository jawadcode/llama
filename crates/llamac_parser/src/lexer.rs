use std::fmt::{self, Display};

use llamac_utils::Span;
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
        match self
            .logos
            .next()
            .map(|(res, range)| (res.unwrap_or(TK::Error), range))
        {
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

#[derive(Debug, Clone, Copy, PartialEq)]
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
#[derive(Debug, Clone, Copy, Logos, PartialEq)]
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
    #[token("+")]  Add,
    #[token("-")]  Sub,
    #[token("*")]  Mul,
    #[token("/")]  Div,
    #[token("+.")] FAdd,
    #[token("-.")] FSub,
    #[token("*.")] FMul,
    #[token("/.")] FDiv,
    #[token("%")]  Mod,
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
    // OTHER
    #[token("ref")] Ref,
    #[token("^")]   Caret,
    
    /* BRACKETS */
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LSquare,
    #[token("]")] RSquare,

    /* MISC */
    #[token("=")]  Bind,
    #[token(":=")] Walrus,
    #[token("|>")] FnPipe,
    #[token("|")]  Pipe,
	#[token(":+")] Append,
    #[token("++")] Concat,
    #[token(",")]  Comma,
    #[token("->")] Arrow,
    #[token("=>")] FatArrow,
    #[token(":")]  Colon,
    #[token(";")]  Semicolon,
    Eof,
        
    #[token("(*", comment_lexer)]
    #[regex(r"[\r\n\t\f\v ]+", logos::skip)]
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
        f.write_str(match self {
            TK::Fun => "'fun'",
            TK::Const => "'const'",
            TK::Let => "'let'",
            TK::Fn => "'fn'",
            TK::If => "'if'",
            TK::Then => "'then'",
            TK::Else => "'else'",
            TK::Cond => "'cond'",
            TK::Match => "'match'",
            TK::Do => "'do'",
            TK::End => "'end'",
            TK::Ident => "identifier",
            TK::UnitLit => "'unit'",
            TK::True => "'true'",
            TK::False => "'false'",
            TK::IntLit => "integer literal",
            TK::StringLit => "string literal",
            TK::FloatLit => "float literal",
            TK::Add => "'+'",
            TK::Sub => "'-'",
            TK::Mul => "'*'",
            TK::Div => "'/'",
            TK::FAdd => "+.",
            TK::FSub => "-.",
            TK::FMul => "*.",
            TK::FDiv => "/.",
            TK::Mod => "'%'",
            TK::Lt => "'<'",
            TK::Leq => "'<='",
            TK::Gt => "'>'",
            TK::Geq => "'>='",
            TK::Eq => "'=='",
            TK::Neq => "'!='",
            TK::Not => "'!'",
            TK::And => "'and'",
            TK::Or => "'or'",
            TK::Ref => "'ref'",
            TK::Caret => "'^'",
            TK::LParen => "''",
            TK::RParen => "')'",
            TK::LSquare => "'['",
            TK::RSquare => "']'",
            TK::Bind => "'='",
            TK::Walrus => "':='",
            TK::FnPipe => "'|>'",
            TK::Pipe => "'|'",
            TK::Append => ":+",
            TK::Concat => "'++'",
            TK::Comma => "','",
            TK::Arrow => "'->'",
            TK::FatArrow => "'=>'",
            TK::Colon => "':'",
            TK::Semicolon => "';'",
            TK::Eof => "EOF",
            TK::Error => "invalid token",
        })
    }
}
