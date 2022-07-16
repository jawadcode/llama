use crate::{
    ast::{SpanStmt, Stmt, TyIdent},
    lexer::TK,
    spanned,
};

use super::{
    error::{ParseResult, SyntaxError},
    Parser,
};

impl Parser<'_> {
    pub(super) fn parse_stmt(&mut self) -> ParseResult<SpanStmt> {
        match self.peek() {
            TK::Fun => self.parse_fundef(),
            TK::Let => self.parse_let(),
            TK::Eof => Err(SyntaxError::End),
            _ => {
                let token = self.next().unwrap();
                Err(SyntaxError::UnexpectedToken {
                    expected: "'let' or 'fun'".to_string(),
                    got: token,
                })
            }
        }
    }

    fn parse_fundef(&mut self) -> ParseResult<SpanStmt> {
        let fun_token = self.next().unwrap();
        let ident = {
            let ident_token = self.consume_next(TK::Ident)?;
            self.text(ident_token).to_string()
        };
        let params = self.parse_params()?;
        let ret_type = if self.at(TK::Colon) {
            self.advance();
            self.parse_type()?
        } else {
            TyIdent::Unit
        };
        let body = Box::new(match self.peek() {
            TK::Assign => {
                self.advance();
                self.expr()
            }
            TK::Do => self.parse_block(),
            _ => {
                let token = self.next().unwrap();
                return Err(SyntaxError::UnexpectedToken {
                    expected: "'=' or 'do'".to_string(),
                    got: token,
                });
            }
        }?);

        Ok(spanned!(
            fun_token.span.start..body.span.end,
            Stmt::FunDef {
                ident,
                params,
                ret_type,
                body
            }
        ))
    }

    fn parse_params(&mut self) -> ParseResult<Vec<(String, TyIdent)>> {
        self.consume(TK::LParen)?;
        let mut args = vec![];
        while !self.at(TK::RParen) {
            let ident = {
                let token = self.consume_next(TK::Ident)?;
                self.text(token).to_string()
            };
            self.consume(TK::Colon)?;
            let ty = self.parse_type()?;
            args.push((ident, ty));

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
        self.consume(TK::RParen)?;

        Ok(args)
    }

    fn parse_type(&mut self) -> ParseResult<TyIdent> {
        let ident = self.consume_next(TK::Ident)?;
        let text = self.text(ident);

        Ok(match text {
            "Number" => TyIdent::Number,
            "String" => TyIdent::String,
            "Bool" => TyIdent::Bool,
            "Unit" => TyIdent::Unit,
            "Fun" => {
                self.consume(TK::LSquare)?;

                let mut inputs = vec![];
                while !self.at(TK::RSquare) {
                    inputs.push(self.parse_type()?);

                    match self.peek() {
                        TK::RSquare => break,
                        TK::Comma => self.advance(),
                        _ => {
                            let token = self.next()?;
                            return Err(SyntaxError::UnexpectedToken {
                                expected: "',' or ']'".to_string(),
                                got: token,
                            });
                        }
                    }
                }

                self.advance();
                self.consume(TK::Arrow)?;
                let output = self.parse_type()?;

                TyIdent::Fun(inputs, Box::new(output))
            }
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "type".to_string(),
                    got: self.next()?,
                })
            }
        })
    }

    fn parse_let(&mut self) -> ParseResult<SpanStmt> {
        let let_token = self.next().unwrap();
        let ident = {
            let ident_token = self.consume_next(TK::Ident)?;
            self.text(ident_token).to_string()
        };
        let annot = if self.at(TK::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.consume(TK::Assign)?;
        let expr = Box::new(self.expr()?);

        Ok(spanned!(
            let_token.span.start..expr.span.end,
            Stmt::Let { ident, annot, expr }
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_stmt {
        ($test:expr, $sexpr:expr) => {
            assert_eq!(Parser::new($test).parse_stmt().unwrap().to_string(), $sexpr)
        };
    }

    #[test]
    fn parse_let() {
        assert_stmt!(
            "let thing = (57489 + 423) * 8989 - 9",
            "(let thing (- (* (+ 57489 423) 8989) 9))"
        );
    }

    #[test]
    fn parse_let_with_annot() {
        assert_stmt!(
            "let thing: Number = (57489 + 423) * 8989 - 9",
            "(let (Number thing) (- (* (+ 57489 423) 8989) 9))"
        );
    }

    #[test]
    fn parse_basic_fndef() {
        assert_stmt!(
            "fun add(x: Number, y: Number) : Number = x + y",
            "(fun add :params [(Number x) (Number y)] :ret_type Number :body (+ x y))"
        );
    }

    #[test]
    fn parse_basic_fndef_no_params() {
        assert_stmt!(
            r#"fun yes() = print("yes")"#,
            r#"(fun yes :params [] :ret_type Unit :body (call print :args ["yes"]))"#
        );
    }

    #[test]
    fn parse_fndef_with_block() {
        assert_stmt!(
            r#"
fun add(x: Number, y: Number) : Number = do
    let thing1 = x + 1;
    let thing2 = y + 2;
    thing1 + thing2
end"#,
            "(fun add :params [(Number x) (Number y)] :ret_type Number :body (block [(let thing1 (+ x 1)) (let thing2 (+ y 2)) (+ thing1 thing2)]))"
        );
    }

    #[test]
    fn parse_fndef_block_sugar() {
        assert_stmt!(
            r#"
fun add(x: Number, y: Number) : Number do
    let thing1 = x + 1;
    let thing2 = y + 2;
    thing1 + thing2
end"#,
            "(fun add :params [(Number x) (Number y)] :ret_type Number :body (block [(let thing1 (+ x 1)) (let thing2 (+ y 2)) (+ thing1 thing2)]))"
        );
    }
}
