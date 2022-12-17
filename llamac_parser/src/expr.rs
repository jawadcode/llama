use llamac_ast::{
    expr::{
        BinOp, BinaryOp, Block, Closure, ClosureParam, ClosureParams, Cond, CondArm, CondArms,
        Expr, FunArgs, FunCall, IfThen, List, ListIndex, Literal, Match, MatchArm, MatchArms,
        MatchPattern, MatchPatterns, SpanExpr, UnOp, UnaryOp,
    },
    spanned,
    utils::Spanned,
    Ident,
};

use crate::{error::SyntaxError, lexer::TK, ParseResult, Parser};

const BIN_OPS: [TK; 16] = [
    TK::Add,
    TK::Sub,
    TK::Star,
    TK::Div,
    TK::Mod,
    TK::And,
    TK::Or,
    TK::Xor,
    TK::Lt,
    TK::Leq,
    TK::Gt,
    TK::Geq,
    TK::Eq,
    TK::Neq,
    TK::FnPipe,
    TK::Concat,
];

const EXPR_TERMINATORS: [TK; 12] = [
    TK::RParen,
    TK::RSquare,
    TK::FatArrow,
    TK::Semicolon,
    TK::Comma,
    TK::Pipe,
    TK::Then,
    TK::Else,
    TK::End,
    TK::Fun,
    TK::Const,
    TK::Eof,
];

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self) -> ParseResult<SpanExpr> {
        let mut lhs = self.parse_operand()?;

        loop {
            let peeked = self.peek();
            let op: BinOp = if BIN_OPS.contains(&peeked) {
                self.lexer.next().unwrap();
                peeked.into()
            } else if EXPR_TERMINATORS.contains(&peeked) {
                break;
            } else {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "operator or expression terminator".to_string(),
                    got: self.next_token()?,
                });
            };

            let rhs = self.parse_operand()?;
            lhs = spanned! {
                lhs.span + rhs.span,
                Box::new(Expr::BinaryOp(BinaryOp {
                    op,
                    lhs,
                    rhs,
                }))
            };
        }

        Ok(lhs)
    }

    fn parse_operand(&mut self) -> ParseResult<SpanExpr> {
        let mut lhs = match self.peek() {
            TK::UnitLit | TK::True | TK::False | TK::IntLit | TK::FloatLit | TK::StringLit => {
                self.parse_lit()?.map(Expr::Literal)
            }
            TK::Ident => self.parse_ident()?.map(Expr::Ident),

            TK::LSquare => self.parse_list()?.map(Expr::List),
            TK::Fn => self.parse_closure()?.map(Expr::Closure),
            TK::If => self.parse_if()?.map(Expr::IfThen),
            TK::Cond => self.parse_cond()?.map(Expr::Cond),
            TK::Match => self.parse_match()?.map(Expr::Match),
            TK::Do => self.parse_block()?.map(Expr::Block),

            TK::LParen => self.parse_grouping()?,
            op @ TK::Sub | op @ TK::Not => self.parse_prefix_op(op)?.map(Expr::UnaryOp),
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_string(),
                    got: self.next_token()?,
                })
            }
        }
        .map(Box::new);

        loop {
            match self.peek() {
                TK::LParen => lhs = self.parse_funcall(lhs)?.map(Expr::FunCall).map(Box::new),
                TK::LSquare => {
                    lhs = self
                        .parse_listindex(lhs)?
                        .map(Expr::ListIndex)
                        .map(Box::new)
                }
                _ => (),
            }

            let peeked = self.peek();
            if BIN_OPS.contains(&peeked) || EXPR_TERMINATORS.contains(&peeked) {
                break;
            } else if [TK::LParen, TK::LSquare].contains(&peeked) {
                continue;
            } else {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "operator or expression terminator".to_string(),
                    got: self.next_token()?,
                });
            }
        }

        Ok(lhs)
    }

    fn parse_lit(&mut self) -> ParseResult<Spanned<Literal>> {
        let token = self.lexer.next().unwrap();
        let text = token.text(self.source);
        let lit = match token.kind {
            TK::UnitLit => Literal::Unit,
            TK::True => Literal::Bool(true),
            TK::False => Literal::Bool(false),
            TK::IntLit => Literal::Int(text.parse::<i64>().unwrap()),
            TK::FloatLit => Literal::Float(text.parse::<f64>().unwrap()),
            TK::StringLit => Literal::String(text[1..(text.len() - 1)].to_string()),
            _ => unreachable!(),
        };
        Ok(spanned! {token.span, lit})
    }

    fn parse_list(&mut self) -> ParseResult<Spanned<List>> {
        let lsquare = self.lexer.next().unwrap();
        let mut list = Vec::new();
        while !self.at(TK::RSquare) {
            let item = self.parse_expr()?;
            list.push(item);
            if !self.at(TK::Comma) {
                break;
            }
        }
        let rsquare = self.expect(TK::RSquare)?;
        Ok(spanned! {lsquare.span + rsquare.span, List(list)})
    }

    fn parse_closure(&mut self) -> ParseResult<Spanned<Closure>> {
        let r#fn = self.lexer.next().unwrap();
        let mut params = Vec::new();
        while !self.at_any([TK::Colon, TK::FatArrow]) {
            let param = if self.at(TK::LParen) {
                let lparen = self.lexer.next().unwrap();
                let name = self.parse_ident()?;
                self.expect(TK::Colon)?;
                let annot = Some(self.parse_type()?);
                let rparen = self.expect(TK::RParen)?;
                spanned! {lparen.span + rparen.span, ClosureParam { name, annot }}
            } else {
                let name = self.parse_ident()?;
                spanned! {name.span, ClosureParam { name, annot: None }}
            };
            params.push(param);
        }
        let params_span = if params.is_empty() {
            (r#fn.span.end..r#fn.span.end).into()
        } else {
            let first = params.first().unwrap();
            let last = params.last().unwrap();
            first.span + last.span
        };
        let ret_ty = if self.at(TK::Colon) {
            self.lexer.next().unwrap();
            Some(self.parse_type()?)
        } else {
            None
        };
        let params = spanned! {params_span, ClosureParams(params)};
        self.expect(TK::FatArrow)?;
        let body = self.parse_expr()?;
        Ok(spanned! {r#fn.span + body.span, Closure { params, ret_ty, body }})
    }

    pub(super) fn parse_if(&mut self) -> ParseResult<Spanned<IfThen>> {
        let r#if = self.lexer.next().unwrap();
        let cond = self.parse_expr()?;
        self.expect(TK::Then)?;
        let then = self.parse_expr()?;
        let mut end = then.span.end;
        let r#else = if self.at(TK::Else) {
            self.lexer.next().unwrap();
            let expr = self.parse_expr()?;
            end = expr.span.end;
            Some(expr)
        } else {
            None
        };
        Ok(spanned! {r#if.span.start..end, IfThen { cond, then, r#else }})
    }

    pub(super) fn parse_cond(&mut self) -> ParseResult<Spanned<Cond>> {
        let cond = self.lexer.next().unwrap();
        let mut arms = Vec::new();
        let mut r#else = None;
        while !self.at(TK::End) {
            let pipe = self.expect(TK::Pipe)?;
            if self.at(TK::Else) {
                self.lexer.next().unwrap();
                self.expect(TK::Arrow)?;
                let branch = self.parse_expr()?;
                r#else = Some(branch);
            } else {
                let cond = self.parse_expr()?;
                self.expect(TK::FatArrow)?;
                let branch = self.parse_expr()?;
                arms.push(spanned! {pipe.span + branch.span, CondArm { cond, branch }});
            }
        }
        let span = {
            let first = arms.first().unwrap();
            let last = arms.last().unwrap();
            first.span + last.span
        };
        let arms = spanned! {span, CondArms(arms)};
        Ok(spanned! {cond.span + span, Cond { arms, r#else }})
    }

    pub(super) fn parse_match(&mut self) -> ParseResult<Spanned<Match>> {
        let r#match = self.lexer.next().unwrap();
        let expr = self.parse_expr()?;
        let mut arms = Vec::new();
        while !self.at(TK::End) {
            let pipe = self.expect(TK::Pipe)?;
            let patterns = self.parse_match_patterns()?;
            self.expect(TK::FatArrow)?;
            let branch = self.parse_expr()?;
            arms.push(spanned! {pipe.span + branch.span, MatchArm { patterns, branch }});
        }
        let span = {
            let first = arms.first().unwrap();
            let last = arms.last().unwrap();
            first.span + last.span
        };
        let arms = spanned! {span, MatchArms(arms)};
        let end = self.expect(TK::End)?;
        Ok(spanned! {r#match.span + end.span, Match { expr, arms }})
    }

    fn parse_match_patterns(&mut self) -> ParseResult<Spanned<MatchPatterns>> {
        let mut pattern = Vec::new();
        while !self.at(TK::FatArrow) {
            let start = self.next_token()?;
            let pat = match start.kind {
                TK::Star => spanned! {start.span, MatchPattern::Wildcard},
                TK::Ident => {
                    let text = start.text(self.source);
                    spanned! {start.span, MatchPattern::NamedWildcard(Ident::new(text))}
                }
                TK::UnitLit | TK::True | TK::False | TK::IntLit | TK::FloatLit | TK::StringLit => {
                    self.parse_lit()?.map(MatchPattern::Literal)
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "match pattern".to_string(),
                        got: start,
                    })
                }
            };
            pattern.push(pat);
            if !self.at(TK::Comma) {
                break;
            }
        }
        let span = {
            let first = pattern.first().unwrap();
            let last = pattern.last().unwrap();
            first.span + last.span
        };
        Ok(spanned! {span, MatchPatterns(pattern)})
    }

    pub(super) fn parse_block(&mut self) -> ParseResult<Spanned<Block>> {
        let r#do = self.lexer.next().unwrap();
        let mut exprs = Vec::new();
        while !self.at(TK::End) {
            if self.at_any([TK::Const, TK::Let, TK::Fun, TK::If, TK::Cond, TK::Match]) {
                let stmt = self.parse_stmt()?;
                exprs.push(spanned! {stmt.span, Box::new(Expr::Stmt(stmt))});
            } else {
                exprs.push(self.parse_expr()?);
            }

            if self.at(TK::Semicolon) {
                self.lexer.next().unwrap();
            } else {
                break;
            }
        }
        let end = self.expect(TK::End)?;
        Ok(spanned! {r#do.span + end.span, Block(exprs)})
    }

    fn parse_grouping(&mut self) -> ParseResult<Spanned<Expr>> {
        let lparen = self.lexer.next().unwrap();
        let expr = self.parse_expr()?;
        let rparen = self.expect(TK::RParen)?;
        Ok(spanned! {lparen.span + rparen.span, *expr.node})
    }

    fn parse_prefix_op(&mut self, op: TK) -> ParseResult<Spanned<UnaryOp>> {
        let op_token = self.lexer.next().unwrap();
        let value = self.parse_expr()?;
        Ok(spanned! {op_token.span + value.span, UnaryOp { op: op.into(), value }})
    }

    fn parse_funcall(&mut self, lhs: SpanExpr) -> ParseResult<Spanned<FunCall>> {
        let fun = lhs;
        let lparen = self.lexer.next().unwrap();
        let mut args = Vec::new();
        while !self.at(TK::RParen) {
            let arg = self.parse_expr()?;
            args.push(arg);

            if self.at(TK::Comma) {
                self.lexer.next().unwrap();
            } else {
                break;
            }
        }
        let rparen = self.expect(TK::RParen)?;
        let args = spanned! {lparen.span + rparen.span, FunArgs(args)};
        Ok(spanned! {fun.span + rparen.span, FunCall { fun, args }})
    }

    fn parse_listindex(&mut self, list: SpanExpr) -> ParseResult<Spanned<ListIndex>> {
        let lsquare = self.lexer.next().unwrap();
        let index = self.parse_expr()?;
        let rsquare = self.lexer.next().unwrap();
        Ok(spanned! {lsquare.span + rsquare.span, ListIndex { list, index }})
    }

    pub(super) fn parse_ident(&mut self) -> ParseResult<Spanned<Ident>> {
        let token = self.expect(TK::Ident)?;
        Ok(spanned! {token.span, Ident::new(token.text(self.source))})
    }
}

impl From<TK> for UnOp {
    fn from(kind: TK) -> Self {
        match kind {
            TK::Not => Self::Not,
            TK::Sub => Self::Negate,
            _ => unreachable!(),
        }
    }
}

impl From<TK> for BinOp {
    fn from(kind: TK) -> Self {
        match kind {
            TK::Add => Self::Add,
            TK::Sub => Self::Sub,
            TK::Star => Self::Mul,
            TK::Div => Self::Div,
            TK::Mod => Self::Mod,
            TK::And => Self::And,
            TK::Or => Self::Or,
            TK::Xor => Self::Xor,
            TK::Lt => Self::Lt,
            TK::Leq => Self::Leq,
            TK::Gt => Self::Gt,
            TK::Geq => Self::Geq,
            TK::Eq => Self::Eq,
            TK::Neq => Self::Neq,
            TK::FnPipe => Self::Pipe,
            TK::Concat => Self::Concat,
            _ => unreachable!(),
        }
    }
}
