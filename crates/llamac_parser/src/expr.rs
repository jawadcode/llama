use std::fmt::{Debug, Display};

use llamac_ast::{
    expr::{
        BinOp, BinaryOp, Block, Closure, ClosureParam, ClosureParams, Cond, CondArm, CondArms,
        Expr, FunArgs, FunCall, IfThen, List, Literal, Match, MatchArm, MatchArms, MatchPattern,
        MatchPatterns, SpanExpr, UnOp, UnaryOp,
    },
    stmt::Stmt,
};

use llamac_utils::{Ident, Spanned, spanned};

use crate::{ParseResult, Parser, error::SyntaxError, lexer::TK};

const BIN_OPS: [TK; 23] = {
    use TK::*;
    [
        Add, Sub, Mul, Div, FAdd, FSub, FMul, FDiv, Mod, Lt, Leq, Gt, Geq, Eq, Neq, Not, And, Or,
        FnPipe, Pipe, Append, Concat, Walrus,
    ]
};

const POSTFIX_OPS: [TK; 1] = {
    use TK::*;
    [Caret]
};

const EXPR_TERMINATORS: [TK; 11] = {
    use TK::*;
    [
        RParen, RSquare, Semicolon, Comma, Pipe, Then, Else, End, Fun, Const, Eof,
    ]
};

const TERM_TOKENS: [TK; 9] = {
    use TK::*;
    [
        UnitLit, True, False, IntLit, FloatLit, StringLit, Ident, LSquare, LParen,
    ]
};

trait Operator {
    fn prefix_bp(&self) -> u8;

    fn infix_bp(&self) -> Option<(u8, u8)>;

    fn postfix_bp(&self) -> Option<u8>;
}

impl Operator for TK {
    /// # Pre-conditions:
    ///
    /// * `self` should be one of `Ref`, `Not`, `Sub`, `FSub` (panic)
    fn prefix_bp(&self) -> u8 {
        match self {
            TK::Ref => 17,
            // Highest binding power
            TK::Not | TK::Sub | TK::FSub => 18,
            _ => unreachable!(),
        }
    }

    fn infix_bp(&self) -> Option<(u8, u8)> {
        Some(match self {
            TK::Walrus => (1, 2),
            TK::Or => (3, 4),
            TK::And => (4, 5),
            TK::Lt | TK::Leq | TK::Gt | TK::Geq | TK::Eq | TK::Neq | TK::FnPipe => (6, 7),
            // Left-associative as it is a snoc and not a cons operation
            // I don't know if this logic is sound lol, we'll see
            TK::Append => (9, 8),
            TK::Add | TK::Sub | TK::FAdd | TK::FSub => (10, 11),
            TK::Mul | TK::Div | TK::FMul | TK::FDiv | TK::Mod => (12, 13),
            TK::Concat => (14, 15),
            // TK::FunApp => (16, 17),
            _ => return None,
        })
    }

    fn postfix_bp(&self) -> Option<u8> {
        match self {
            TK::Caret => Some(69),
            _ => None,
        }
    }
}

impl Parser<'_> {
    #[inline]
    pub(super) fn parse_expr(&mut self) -> ParseResult<SpanExpr> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, power: u8) -> ParseResult<SpanExpr> {
        let mut lhs = self.parse_term()?;

        loop {
            let op = if self.at_any(EXPR_TERMINATORS) {
                break;
            } else if self.at_any(BIN_OPS) || self.at_any(POSTFIX_OPS) || self.at_any(TERM_TOKENS) {
                self.peek()
            } else {
                let token = self.next_token().unwrap();
                return Err(SyntaxError::UnexpectedToken {
                    expected: "operator or expression terminator".to_string(),
                    got: token,
                });
            };

            if let Some(left_bp) = op.postfix_bp() {
                if left_bp < power {
                    break;
                }

                let op = self.next_token().unwrap();

                lhs = spanned! {
                    lhs.span + op.span,
                    Box::new(Expr::UnaryOp(UnaryOp {
                        op: spanned!{op.span, op.kind.into()},
                        value: lhs,
                    }))
                };
                continue;
            }

            if let Some((left_bp, right_bp)) = op.infix_bp() {
                if left_bp < power {
                    break;
                }
                let op = self.next_token().unwrap();
                let rhs = self.parse_expr_bp(right_bp)?;
                lhs = spanned! {
                    lhs.span + rhs.span,
                    Box::new(Expr::BinaryOp(BinaryOp {
                        op: spanned!{op.span, op.kind.into()},
                        lhs,
                        rhs,
                    }))
                };
                continue;
            }

            let mut args = Vec::new();
            while self.at_any(TERM_TOKENS) {
                if 15 < power {
                    break;
                }

                args.push(self.parse_expr_bp(16)?);
            }
            if !args.is_empty() {
                let last_arg_span = args.last().unwrap().span;
                lhs = spanned! {
                    lhs.span + last_arg_span,
                    Box::new(Expr::FunCall(FunCall {
                        fun: lhs,
                        args: spanned!{
                            args.first().unwrap().span + last_arg_span,
                            FunArgs(args)
                        }
                    }))
                };
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_term(&mut self) -> ParseResult<SpanExpr> {
        Ok(match self.peek() {
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
            TK::Ref | TK::Sub | TK::FSub | TK::Not => self.parse_prefix_op()?.map(Expr::UnaryOp),
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_string(),
                    got: self.next_token()?,
                });
            }
        }
        .map(Box::new))
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
            lit => panic!("{lit}"),
        };
        Ok(spanned! {token.span, lit})
    }

    fn parse_list(&mut self) -> ParseResult<Spanned<List>> {
        let lsquare = self.lexer.next().unwrap();
        let mut list = Vec::new();
        while !self.at(TK::RSquare) {
            let item = self.parse_expr()?;
            list.push(item);
            if self.at(TK::Comma) {
                self.next_token().unwrap();
            } else {
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
                self.expect(TK::FatArrow)?;
                let target = self.parse_expr()?;
                r#else = Some(target);
            } else {
                let cond = self.parse_expr()?;
                self.expect(TK::FatArrow)?;
                let target = self.parse_expr()?;
                arms.push(spanned! {pipe.span + target.span, CondArm { cond, target }});
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
        let examinee = self.parse_expr()?;
        let mut arms = Vec::new();
        while !self.at(TK::End) {
            let pipe = self.expect(TK::Pipe)?;
            let patterns = self.parse_match_patterns()?;
            self.expect(TK::FatArrow)?;
            let target = self.parse_expr()?;
            arms.push(spanned! {pipe.span + target.span, MatchArm { patterns, target }});
        }
        let span = {
            let first = arms.first().unwrap();
            let last = arms.last().unwrap();
            first.span + last.span
        };
        let arms = spanned! {span, MatchArms(arms)};
        let end = self.expect(TK::End)?;
        Ok(spanned! {r#match.span + end.span, Match { examinee, arms }})
    }

    fn parse_match_patterns(&mut self) -> ParseResult<Spanned<MatchPatterns>> {
        let mut pattern = Vec::new();
        while !self.at(TK::FatArrow) {
            let pat = match self.peek() {
                TK::Mul => {
                    let tok = self.next_token().unwrap();
                    spanned! {tok.span, MatchPattern::Wildcard}
                }
                TK::Ident => {
                    let tok = self.next_token().unwrap();
                    let text = tok.text(self.source);
                    spanned! {tok.span, MatchPattern::NamedWildcard(Ident::new(text))}
                }
                TK::UnitLit | TK::True | TK::False | TK::IntLit | TK::FloatLit | TK::StringLit => {
                    self.parse_lit()?.map(MatchPattern::Literal)
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "match pattern".to_string(),
                        got: self.next_token()?,
                    });
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
        let mut tail = None;

        while !self.at(TK::End) {
            let (expr, is_tail) = if self.at_any([TK::Const, TK::Let, TK::Fun]) {
                let stmt = self.parse_stmt()?;
                (
                    spanned! {stmt.span, Box::new(Expr::Stmt(stmt))},
                    self.at(TK::End),
                )
            } else if self.at_any([TK::If, TK::Cond, TK::Match]) {
                let peeked = self.peek();
                match peeked {
                    TK::If => {
                        let if_then = self.parse_if()?;
                        self.parse_control_flow(if_then, Expr::IfThen, Stmt::IfThen)
                    }
                    TK::Cond => {
                        let cond = self.parse_cond()?;
                        self.parse_control_flow(cond, Expr::Cond, Stmt::Cond)
                    }
                    TK::Match => {
                        let r#match = self.parse_match()?;
                        self.parse_control_flow(r#match, Expr::Match, Stmt::Match)
                    }
                    _ => unreachable!(),
                }
            } else {
                let expr = self.parse_expr()?;
                (expr, self.at(TK::End))
            };

            if is_tail {
                tail = Some(expr);
            } else {
                exprs.push(expr);
            }

            if self.at(TK::Semicolon) {
                self.lexer.next().unwrap();
            } else {
                break;
            }
        }
        let end = self.expect(TK::End)?;
        Ok(spanned! {r#do.span + end.span, Block { exprs, tail }})
    }

    fn parse_control_flow<T: Clone + Debug + Display>(
        &mut self,
        cflow: Spanned<T>,
        expr_ctor: fn(T) -> Expr,
        stmt_ctor: fn(T) -> Stmt,
    ) -> (SpanExpr, bool) {
        if self.at(TK::End) {
            (cflow.map(|cflow| Box::new(expr_ctor(cflow))), true)
        } else {
            (
                spanned! {
                    cflow.span,
                    Box::new(Expr::Stmt(
                        spanned! {
                            cflow.span,
                            stmt_ctor(cflow.node)
                        }
                    ))
                },
                false,
            )
        }
    }

    fn parse_grouping(&mut self) -> ParseResult<Spanned<Expr>> {
        let lparen = self.lexer.next().unwrap();
        let expr = self.parse_expr()?;
        let rparen = self.expect(TK::RParen)?;
        Ok(spanned! {lparen.span + rparen.span, *expr.node})
    }

    fn parse_prefix_op(&mut self) -> ParseResult<Spanned<UnaryOp>> {
        let op = self.lexer.next().unwrap();
        let right_bp = op.kind.prefix_bp();
        let value = self.parse_expr_bp(right_bp)?;
        Ok(spanned! {
            op.span + value.span,
            UnaryOp { op: spanned!{op.span, op.kind.into()}, value }
        })
    }

    pub(super) fn parse_ident(&mut self) -> ParseResult<Spanned<Ident>> {
        let token = self.expect(TK::Ident)?;
        Ok(spanned! {token.span, Ident::new(token.text(self.source))})
    }
}

impl From<TK> for UnOp {
    fn from(kind: TK) -> Self {
        match kind {
            TK::Ref => Self::Ref,
            TK::Caret => Self::Deref,
            TK::Not => Self::Not,
            TK::Sub => Self::INegate,
            TK::FSub => Self::FNegate,
            _ => unreachable!(),
        }
    }
}

impl From<TK> for BinOp {
    fn from(kind: TK) -> Self {
        match kind {
            TK::Add => Self::Add,
            TK::Sub => Self::Sub,
            TK::Mul => Self::Mul,
            TK::Div => Self::Div,
            TK::FAdd => Self::FAdd,
            TK::FSub => Self::FSub,
            TK::FMul => Self::FMul,
            TK::FDiv => Self::FDiv,
            TK::Mod => Self::Mod,
            TK::And => Self::And,
            TK::Or => Self::Or,
            TK::Lt => Self::Lt,
            TK::Leq => Self::Leq,
            TK::Gt => Self::Gt,
            TK::Geq => Self::Geq,
            TK::Eq => Self::Eq,
            TK::Neq => Self::Neq,
            TK::FnPipe => Self::Pipe,
            TK::Concat => Self::Concat,
            TK::Append => Self::Append,
            TK::Walrus => Self::Assign,
            _ => unreachable!(),
        }
    }
}
