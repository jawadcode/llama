use std::{iter::Peekable, path::Path};

use crate::{
    ast::SourceFile,
    lexer::{Lexer, Token, TK},
};

use self::error::{ParseResult, SyntaxError};

pub mod error;
pub mod expr;
pub mod stmt;

/// The parser and all associated state
pub struct Parser<'input> {
    input: &'input str,
    lexer: Peekable<Lexer<'input>>,
}

#[macro_export]
macro_rules! spanned {
    ($span:expr, $node:expr) => {
        $crate::ast::Spanned {
            span: $span.into(),
            node: $node,
        }
    };
}

impl<'input> Parser<'input> {
    /// Create a new parser that operates on the given `input`
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input).peekable(),
        }
    }

    /// Parse `self.input` as a file, the `path` *must* be valid
    pub fn parse_file(&mut self, path: &Path) -> Option<SourceFile> {
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut defs = vec![];
        loop {
            match self.parse_stmt() {
                Ok(stmt) => defs.push(stmt),
                Err(err) => match err {
                    SyntaxError::End => break,
                    err => {
                        err.report(filename.to_string(), self.input);
                        return None;
                    }
                },
            }
        }

        Some(SourceFile {
            path: path.canonicalize().unwrap(),
            defs,
        })
    }

    /// Return the next token, or if there are no more tokens: `SyntaxError::UnexpectedEof`
    fn next(&mut self) -> ParseResult<Token> {
        self.lexer.next().ok_or_else(|| {
            let len = self.input.len();
            SyntaxError::UnexpectedEof(Token {
                kind: TK::Eof,
                span: (len..len).into(),
            })
        })
    }

    /// Peek the `kind` of the next token without consuming
    fn peek(&mut self) -> TK {
        self.lexer.peek().map(|token| token.kind).unwrap_or(TK::Eof)
    }

    /// Consume the next token and check that its kind is as `expected`, returning
    /// `SyntaxError::UnexpectedToken` if not
    fn consume(&mut self, expected: TK) -> ParseResult<()> {
        let token = self.next()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                got: token,
            })
        } else {
            Ok(())
        }
    }

    /// Checks that the next token's kind is as `expected` and also returns the token
    fn consume_next(&mut self, expected: TK) -> ParseResult<Token> {
        let token = self.next()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                got: token,
            })
        } else {
            Ok(token)
        }
    }

    /// Advance without checking anything
    fn advance(&mut self) {
        self.lexer.next().unwrap();
    }

    /// Peek the next token and check if its kind is `kind`
    fn at(&mut self, kind: TK) -> bool {
        self.peek() == kind
    }

    /// Peek the next token and check if its kind is one of `kinds`
    fn at_any<const N: usize>(&mut self, kinds: [TK; N]) -> bool {
        kinds.contains(&self.peek())
    }

    /// Obtain source text behind given token
    fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }
}
