use std::{iter::Peekable, path::Path};

use error::SyntaxError;
use lexer::{Lexer, Token, TK};
use llamac_ast::SourceFile;

pub mod error;
pub mod expr;
pub mod item;
pub mod lexer;
pub mod stmt;

pub struct Parser<'source> {
    source: &'source str,
    lexer: Peekable<Lexer<'source>>,
}

pub type ParseResult<T> = Result<T, SyntaxError>;

impl<'source> Parser<'source> {
    /// Create a new parser which operates on `source`
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source).peekable(),
        }
    }

    /// Parse into a `llamac_ast::SourceFile`
    pub fn parse_file(&mut self, path: &Path) -> ParseResult<SourceFile> {
        let mut items = Vec::new();
        while let Some(result) = self.next() {
            match result {
                Ok(item) => {
                    items.push(item);
                }
                err @ Err(_) => {
                    err?;
                }
            }
        }

        Ok(SourceFile {
            path: path.canonicalize().unwrap(),
            items,
        })
    }

    fn next_token(&mut self) -> ParseResult<Token> {
        self.lexer.next().ok_or_else(|| {
            let len = self.source.len();
            SyntaxError::UnexpectedEof((len..len).into())
        })
    }

    fn peek(&mut self) -> TK {
        self.lexer.peek().map(|tok| tok.kind).unwrap_or(TK::Eof)
    }

    fn at(&mut self, expected: TK) -> bool {
        self.peek() == expected
    }

    fn at_any<const N: usize>(&mut self, expected: [TK; N]) -> bool {
        expected.contains(&self.peek())
    }

    fn expect(&mut self, expected: TK) -> ParseResult<Token> {
        let token = self.next_token()?;
        if token.kind == expected {
            Ok(token)
        } else {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                got: token,
            })
        }
    }
}
