use std::iter::Peekable;

use crate::lexer::{Lexer, Token, TK};

use self::error::{ParseResult, SyntaxError};

pub mod error;

pub struct Parser<'input> {
    input: &'input str,
    lexer: Peekable<Lexer<'input>>,
}

impl<'input> Parser<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input).peekable(),
        }
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
