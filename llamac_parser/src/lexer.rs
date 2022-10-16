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
    #[token("let")]   Let,
    #[token("fn")]    Fn,
    #[token("if")]    If,
    #[token("then")]  Then,
    #[token("else")]  Else,
    #[token("case")]  Case,
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
    #[token("*")] Mul,
    #[token("/")] Div,
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
    
    /* BRACKETS */
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LSquare,
    #[token("]")] RSquare,

    /* MISC */
    #[token("=")]  Assign,
    #[token("|>")] FnPipe,
    #[token("|")]  Pipe,
    #[token(",")]  Comma,
    #[token("->")] Arrow,
    #[token("=>")] FatArrow,
    #[token(":")]  Colon,
    #[token(";")]  Semicolon,
    Eof,
        
    #[token("(*", comment_lexer)]
    #[regex(r"[ \t\r\n\f]+")]
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
                    lex.bump(idx + 2);
                    return Skip;
                }
                _ => {}
            }
        }

        previous = Some(current);
    }

    Skip
}
