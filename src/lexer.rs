use std::ops::{Index, Range};

use derive_more::Display;
use logos::{Logos, Skip, SpannedIter};

#[derive(Debug, Display, Clone, Copy, Logos, PartialEq)]
// The *T*oken *K*inds
pub enum TK {
    /* KEYWORDS */
    #[token("fun")]
    #[display(fmt = "fun")]
    Fun,
    #[token("let")]
    #[display(fmt = "let")]
    Let,
    #[token("fn")]
    #[display(fmt = "fn")]
    Fn,
    #[token("if")]
    #[display(fmt = "if")]
    If,
    #[token("then")]
    #[display(fmt = "then")]
    Then,
    #[token("else")]
    #[display(fmt = "else")]
    Else,
    #[token("match")]
    #[display(fmt = "match")]
    Match,
    #[token("do")]
    #[display(fmt = "do")]
    Do,
    #[token("end")]
    #[display(fmt = "end")]
    End,
    /* LITERALS */
    #[regex(r"([A-Za-z]|_)([A-Za-z]|_|\d)*")]
    #[display(fmt = "identifier")]
    Ident,
    #[token("()")]
    #[display(fmt = "()")]
    Unit,
    #[token("true")]
    #[display(fmt = "true")]
    True,
    #[token("false")]
    #[display(fmt = "false")]
    False,
    #[regex("[0-9]+", priority = 2)]
    #[regex("0x[a-f0-9]+")]
    #[regex("0o[0-7]+")]
    #[regex("0b[0-1]+")]
    #[display(fmt = "integer literal")]
    IntLit,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    #[display(fmt = "float literal")]
    FloatLit,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    #[display(fmt = "string literal")]
    StringLit,
    /* ARITHMETIC OPERATORS */
    #[token("+")]
    #[display(fmt = "+")]
    Add,
    #[token("-")]
    #[display(fmt = "-")]
    Sub,
    #[token("*")]
    #[display(fmt = "*")]
    Mul,
    #[token("/")]
    #[display(fmt = "/")]
    Div,
    /* COMPARISON OPERATORS */
    #[token("<")]
    #[display(fmt = "<")]
    Less,
    #[token("<=")]
    #[display(fmt = "<=")]
    LessEq,
    #[token(">")]
    #[display(fmt = ">")]
    Greater,
    #[token(">=")]
    #[display(fmt = ">=")]
    GreatEq,
    #[token("==")]
    #[display(fmt = "==")]
    Eq,
    #[token("!=")]
    #[display(fmt = "!=")]
    NotEq,
    /* BOOLEAN OPERATORS */
    #[token("!")]
    #[display(fmt = "!")]
    Not,
    #[token("and")]
    #[display(fmt = "and")]
    And,
    #[token("or")]
    #[display(fmt = "or")]
    Or,
    /* BRACKETS */
    #[token("(")]
    #[display(fmt = "(")]
    LeftParen,
    #[token(")")]
    #[display(fmt = ")")]
    RightParen,
    /* MISC */
    #[token("=")]
    #[display(fmt = "=")]
    Assign,
    #[token("|>")]
    #[display(fmt = "|>")]
    FnPipe,
    #[token("|")]
    #[display(fmt = "|")]
    Pipe,
    #[token(",")]
    #[display(fmt = ",")]
    Comma,
    #[token("=>")]
    #[display(fmt = "=>")]
    FatArrow,
    #[token(";")]
    #[display(fmt = ";")]
    Semicolon,
    #[display(fmt = "EOF")]
    Eof,
    #[token("(*", comment_lexer)]
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[error]
    Error,
}

fn comment_lexer(lex: &mut logos::Lexer<TK>) -> Skip {
    let rem = lex.remainder();

    let (mut nesting, mut previous) = (0, None);
    for (idx, current) in rem.char_indices() {
        if let Some(previous) = previous {
            match (current, previous) {
                ('*', '(') => nesting += 1,
                (')', '*') if nesting > 0 => nesting -= 1,
                (')', '*') => {
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

#[derive(Debug, Display, Clone, Copy, PartialEq)]
#[display(fmt = "{}", kind)]
/// Holds the kind of token for parsing, and the span to extract it's text from the source code
pub struct Token {
    /// The type of token
    pub kind: TK,
    /// The position of the `Token` in the source code
    pub span: Span,
}

impl Token {
    #[inline]
    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
#[display(fmt = "{}..{}", start, end)]
/// Custom span for storing the position of a token or AST node in the source string
pub struct Span {
    /// The start of the span (inclusive)
    pub start: usize,
    /// The end of the span (exclusive)
    pub end: usize,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

/// A wrapper around `logos::SpannedIter` to map to our custom `Token` type and also to map `None`
/// to `TK::Eof` to allow for easier Eof handling while parsing
pub struct Lexer<'input> {
    /// The length of the input string so the EOF `Token` can have a correct span
    length: usize,
    logos: SpannedIter<'input, TK>,
    eof: bool,
}

impl<'input> Lexer<'input> {
    /// Create a new `Lexer` that lazily lexes `input`
    pub fn new(input: &'input str) -> Self {
        Self {
            length: input.len(),
            logos: TK::lexer(input).spanned(),
            eof: false,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
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
