use crate::{
    ast::{Expr, Lit, MatchArm, SpanExpr},
    lexer::{Token, TK},
    spanned,
};

use super::{
    error::{ParseResult, SyntaxError},
    Parser,
};

const BASIC_EXPR_TOKENS: [TK; 7] = [
    TK::LParen,
    TK::True,
    TK::False,
    TK::IntLit,
    TK::FloatLit,
    TK::StringLit,
    TK::Ident,
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
            TK::LParen => self.parse_group(),

            op @ TK::Sub | op @ TK::Not => self.parse_prefix_op(op),
            _ => todo!(),
        }?;

        loop {
            if self.at(TK::LParen) {
                lhs = self.parse_fncall(lhs)?;
            }

            let op = match self.peek() {
                op @ TK::Add
                | op @ TK::Sub
                | op @ TK::Mul
                | op @ TK::Div
                | op @ TK::Less
                | op @ TK::LessEq
                | op @ TK::Greater
                | op @ TK::GreatEq
                | op @ TK::Eq
                | op @ TK::NotEq
                | op @ TK::Not
                | op @ TK::And
                | op @ TK::Or => op,

                // These tokens are expression terminators, if one of these appear after an
                // expression, the expression is fully parsed and there are no operators after it
                TK::RParen
                | TK::Let
                | TK::Fun
                | TK::FatArrow
                | TK::Semicolon
                | TK::Comma
                | TK::Pipe
                | TK::Then
                | TK::Else
                | TK::End
                | TK::Eof => break,

                _ => {
                    let token = self.next()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "operator or expression terminator".to_string(),
                        got: token,
                    });
                }
            };

            // Parse operator expression
            if let Some((left_bp, ())) = op.postfix_bp() {
                if left_bp < bp {
                    // previous operator has a higher binding power than the new one
                    // => end of expression
                    break;
                }

                let op_token = self.next().unwrap();

                // no recursive call here, because we have already parsed our operand `lhs`
                lhs = spanned!(
                    lhs.span.start..op_token.span.end,
                    Expr::UnOp {
                        op,
                        operand: Box::new(lhs)
                    }
                );

                // parsed an operator => go round the loop again
                continue;
            }

            if let Some((left_bp, right_bp)) = op.infix_bp() {
                if left_bp < bp {
                    // previous operator has a higher binding power than the new one
                    // => end of expression
                    break;
                }

                self.advance();

                let rhs = self.parse_expr(right_bp)?;
                lhs = spanned!(
                    lhs.span.start..rhs.span.end,
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs)
                    }
                );

                // parsed an operator => go round the loop again
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    /// Parse string, float, int, boolean, and unit literals
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
                            b as char
                        )))
                    }
                };
                backslash = false;
            } else if byte == b'\\' {
                backslash = true;
            } else {
                buf.push(byte);
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
            fn_token.span.start..body.span.end,
            Expr::Closure { params, body }
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
            if_token.span.start..elss.span.end,
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
            while self.at_any(BASIC_EXPR_TOKENS) {
                let peeked = self.peek();
                patterns.push(self.parse_basic_expr(peeked)?);
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
            match_token.span.start..end.span.end,
            Expr::Match { expr, arms }
        ))
    }

    /// Parse basic expression
    fn parse_basic_expr(&mut self, peeked: TK) -> ParseResult<SpanExpr> {
        match peeked {
            lit @ TK::Unit
            | lit @ TK::True
            | lit @ TK::False
            | lit @ TK::IntLit
            | lit @ TK::FloatLit
            | lit @ TK::StringLit => self.parse_lit(lit),

            TK::Ident => self.parse_ident(),

            _ => {
                let token = self.next()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "literal or identifier".to_string(),
                    got: token,
                })
            }
        }
    }

    /// Parse block expression
    pub(super) fn parse_block(&mut self) -> ParseResult<SpanExpr> {
        let do_token = self.next().unwrap();
        let mut exprs = vec![];
        while !self.at(TK::End) {
            if self.at_any([TK::Let, TK::Fun]) {
                exprs.push(self.parse_stmt_expr()?);
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
            do_token.span.start..end.span.end,
            Expr::Block { exprs }
        ))
    }

    /// Parse a statement expression
    fn parse_stmt_expr(&mut self) -> ParseResult<SpanExpr> {
        let stmt = self.parse_stmt()?;
        Ok(spanned!(stmt.span, Expr::Stmt(stmt)))
    }

    /// Parse grouping
    fn parse_group(&mut self) -> ParseResult<SpanExpr> {
        let lparen = self.next().unwrap();
        let expr = self.expr()?;
        let rparen = self.consume_next(TK::RParen)?;

        Ok(spanned!(lparen.span.start..rparen.span.end, expr.node))
    }

    /// Parse prefix operator expression
    fn parse_prefix_op(&mut self, op: TK) -> ParseResult<SpanExpr> {
        let op_token = self.next().unwrap();
        let ((), right_bp) = op.prefix_bp().unwrap();

        let operand = Box::new(self.parse_expr(right_bp)?);
        Ok(spanned!(
            op_token.span.start..operand.span.end,
            Expr::UnOp { op, operand }
        ))
    }

    /// Parse function call
    fn parse_fncall(&mut self, lhs: SpanExpr) -> ParseResult<SpanExpr> {
        self.advance();

        let mut args = vec![];
        while !self.at(TK::RParen) {
            args.push(self.expr()?);

            match self.peek() {
                TK::RParen => break,
                TK::Comma => self.advance(),
                _ => {
                    let token = self.next()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "',' or ')'".to_string(),
                        got: token,
                    });
                }
            }
        }
        let end = self.consume_next(TK::RParen)?;

        Ok(spanned!(
            lhs.span.start..end.span.end,
            Expr::Call {
                fun: Box::new(lhs),
                args
            }
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_expr {
        ($test:expr, $sexpr:expr) => {
            assert_eq!(Parser::new($test).expr().unwrap().to_string(), $sexpr)
        };
    }

    #[test]
    fn parse_bool_expr() {
        assert_expr!(
            "!true or 10 < 6 and 2 != 1",
            "(or (! true) (and (< 10 6) (!= 2 1)))"
        );
    }

    #[test]
    fn parse_arithmetic_comparison_operators() {
        assert_expr!(
            "10 + -9 * 0 - 90 / -90 != 69 / -4",
            "(!= (- (+ 10 (* (- 9) 0)) (/ 90 (- 90))) (/ 69 (- 4)))"
        );
    }

    #[test]
    fn parse_identifier() {
        assert_expr!("hello", "hello");
    }

    #[test]
    fn parse_simple_function_call() {
        assert_expr!("add(1, 69 + 420, 2)", "(call add :args [1 (+ 69 420) 2])");
    }

    #[test]
    fn parse_nested_function_call() {
        assert_expr!(
            "add(69 + 420, add(57893, 43280))",
            "(call add :args [(+ 69 420) (call add :args [57893 43280])])"
        );
    }

    #[test]
    fn parse_empty_function_call() {
        assert_expr!("hello()", "(call hello :args [])");
    }

    #[test]
    fn parse_trailing_comma_function_call() {
        assert_expr!("add(1, 2,)", "(call add :args [1 2])");
    }

    #[test]
    fn parse_if_expr() {
        assert_expr!(
            "if 10 > 89 + 90 then 1 + 1 else -1",
            "(if :cond (> 10 (+ 89 90)) :then (+ 1 1) :else (- 1))"
        );
    }

    #[test]
    fn parse_match_expr() {
        assert_expr!(
            "
match x
  | 1    => 69
  | 2, 3 => 420
  | _    => x * x * x
end",
            "(match x :arms [([1] 69) ([2 3] 420) ([_] (* (* x x) x))])"
        );
    }

    #[test]
    fn parse_closure() {
        assert_expr!("fn x => x + 1", "(fn :params [x] :body (+ x 1))");
    }

    #[test]
    fn parse_block_expr() {
        assert_expr!(
            r#"
do
  let thing = 123;
  print("lol");
  let thing2 = 234;
  thing + thing2
end"#,
            r#"(block [(let thing 123) (call print :args ["lol"]) (let thing2 234) (+ thing thing2)])"#
        );
    }
}
