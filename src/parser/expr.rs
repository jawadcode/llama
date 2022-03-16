use crate::{
    ast::{Boxpr, Expr, Function, Lit, MatchArm, SpanExpr},
    lexer::{Token, TK},
};

use super::{
    error::{ParseResult, SyntaxError},
    Parser,
};

const LITERAL_TOKENS: [TK; 6] = [
    TK::Unit,
    TK::True,
    TK::False,
    TK::IntLit,
    TK::FloatLit,
    TK::StringLit,
];

/// To obtain the binding power(s) of an operator
trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_bp(&self) -> Option<((), u8)>;

    /// Infix operators bind two operands, lhs and rhs
    fn infix_bp(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_bp(&self) -> Option<(u8, ())>;
}

impl Operator for TK {
    fn prefix_bp(&self) -> Option<((), u8)> {
        Some(match self {
            TK::Sub => ((), 51),
            TK::Not => ((), 101),
            _ => {
                return None;
            }
        })
    }

    fn infix_bp(&self) -> Option<(u8, u8)> {
        Some(match self {
            TK::Or => (1, 2),
            TK::And => (3, 4),
            TK::Eq | TK::NotEq => (5, 6),
            TK::Less | TK::Greater | TK::LessEq | TK::GreatEq => (7, 8),
            TK::Add | TK::Sub => (9, 10),
            TK::Mul | TK::Div => (11, 12),
            _ => return None,
        })
    }

    fn postfix_bp(&self) -> Option<(u8, ())> {
        // *currently*, we have 0 postfix operators
        None
    }
}

macro_rules! spanned {
    ($span:expr, $node:expr) => {
        $crate::ast::Spanned {
            span: $span.into(),
            node: $node,
        }
    };
}

impl Parser<'_> {
    /// Parse an expression with an initial binding power of 0
    pub fn expr(&mut self) -> ParseResult<SpanExpr> {
        self.parse_expr(0)
    }

    /// Parse an expression with an initial binding power of `bp`
    fn parse_expr(&mut self, bp: u8) -> ParseResult<SpanExpr> {
        let mut lhs = match self.peek() {
            lit @ TK::Unit
            | lit @ TK::True
            | lit @ TK::False
            | lit @ TK::IntLit
            | lit @ TK::FloatLit
            | lit @ TK::StringLit => self.parse_lit(lit),

            TK::Ident => self.parse_ident(),
            TK::Fn => self.parse_closure(),
            TK::If => self.parse_if(),
            TK::Match => self.parse_match(),
            TK::Do => self.parse_block(),
            TK::LeftParen => self.parse_group(),
            _ => todo!(),
        }?;

        Ok(lhs)
    }

    /// Parse any literal expression
    fn parse_lit(&mut self, lit: TK) -> ParseResult<SpanExpr> {
        let token = self.next().unwrap();
        let text = self.text(token);

        let lit = match lit {
            TK::StringLit => Lit::Str(Self::parse_string(token, text)?),
            TK::FloatLit => Lit::Float(
                lexical::parse::<f64, _>(text).map_err(|_| SyntaxError::InvalidLiteral(token))?,
            ),
            TK::IntLit => Lit::Int(
                lexical::parse::<i64, _>(text).map_err(|_| SyntaxError::InvalidLiteral(token))?,
            ),
            b @ TK::True | b @ TK::False => Lit::Bool(b == TK::True),
            TK::Unit => Lit::Unit,
            _ => unreachable!(),
        };

        Ok(spanned!(token.span, Expr::Lit(lit)))
    }

    /// Parse string literal, including escape sequences
    fn parse_string(token: Token, text: &str) -> ParseResult<String> {
        let mut buf = vec![];
        let mut backslash = false;
        for (i, byte) in text[1..(text.len() - 1)].bytes().enumerate() {
            if backslash {
                match byte {
                    b't' => buf.push(b'\t'),
                    b'n' => buf.push(b'\n'),
                    b @ b'"' | b @ b'\\' => buf.push(b),
                    b => {
                        return Err(SyntaxError::InvalidEscSeq(spanned!(
                            (i..(i + 2)),
                            char::from(b)
                        )))
                    }
                };
                backslash = false;
            } else {
                if byte == b'\\' {
                    backslash = true;
                } else {
                    buf.push(byte);
                }
            }
        }

        String::from_utf8(buf).map_err(|_| SyntaxError::InvalidLiteral(token))
    }

    /// Parse identifier
    fn parse_ident(&mut self) -> ParseResult<SpanExpr> {
        let token = self.next().unwrap();
        let text = self.text(token);

        Ok(spanned!(token.span, Expr::Ident(text.to_string())))
    }

    /// Parse closure
    fn parse_closure(&mut self) -> ParseResult<SpanExpr> {
        let fn_token = self.next().unwrap();
        let mut params = vec![];
        while self.at(TK::Ident) {
            let text = {
                let token = self.next().unwrap();
                self.text(token)
            };
            params.push(text.to_string());
        }
        self.consume(TK::FatArrow)?;

        let body = Box::new(self.expr()?);
        Ok(spanned!(
            (fn_token.span.start..body.span.end),
            Expr::Closure(Function {
                ident: None,
                params,
                body
            })
        ))
    }

    /// Parse if expression
    fn parse_if(&mut self) -> ParseResult<SpanExpr> {
        let if_token = self.next().unwrap();
        let cond = Box::new(self.expr()?);
        self.consume(TK::Then)?;
        let then = Box::new(self.expr()?);
        self.consume(TK::Else)?;
        let elss = Box::new(self.expr()?);

        Ok(spanned!(
            (if_token.span.start..elss.span.end),
            Expr::If { cond, then, elss }
        ))
    }

    /// Parse match expression
    fn parse_match(&mut self) -> ParseResult<SpanExpr> {
        let match_token = self.next().unwrap();
        let expr = Box::new(self.expr()?);

        let mut arms = vec![];
        while self.at(TK::Pipe) {
            self.advance();

            let mut patterns = vec![];
            while self.at_any(LITERAL_TOKENS) {
                let peeked = self.peek();
                patterns.push(self.parse_lit(peeked)?);
                if self.at(TK::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.consume(TK::FatArrow)?;
            arms.push(MatchArm {
                patterns,
                expr: self.expr()?,
            })
        }
        let end = self.consume_next(TK::End)?;

        Ok(spanned!(
            (match_token.span.start..end.span.end),
            Expr::Match { expr, arms }
        ))
    }

    /// Parse block expression
    fn parse_block(&mut self) -> ParseResult<SpanExpr> {
        let do_token = self.next().unwrap();
        let mut exprs = vec![];
        while !self.at(TK::End) {
            if self.at_any([TK::Let, TK::Fun]) {
                exprs.push(todo!());
            } else {
                exprs.push(self.expr()?);
            }

            if self.at(TK::Semicolon) {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.consume_next(TK::End)?;

        Ok(spanned!(
            (do_token.span.start..end.span.end),
            Expr::Block { exprs }
        ))
    }

    /// Parse grouping
    fn parse_group(&mut self) -> ParseResult<SpanExpr> {
        let l_token = self.next().unwrap();
        let expr = self.expr()?;
        let r_token = self.consume_next(TK::RightParen)?;

        Ok(spanned!((l_token.span.start..r_token.span.end), expr.node))
    }
}
