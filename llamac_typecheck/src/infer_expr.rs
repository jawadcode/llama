use llamac_ast::{
    expr::{
        BinOp, BinaryOp, Block, Closure, Cond, CondArm, Expr, FunArgs, FunCall, IfThen, List,
        Literal, Match, MatchArm, MatchArms, MatchPattern, SpanExpr, UnOp, UnaryOp,
    },
    stmt::SpanStmt,
};
use llamac_typed_ast::{
    Type, Types,
    expr::{
        InnerExpr, TypedBinaryOp, TypedBlock, TypedClosure, TypedClosureParam, TypedClosureParams,
        TypedCondArm, TypedCondArms, TypedCondExpr, TypedExpr, TypedFunArgs, TypedFunCall,
        TypedIfThenExpr, TypedList, TypedMatch, TypedMatchArm, TypedMatchArms, TypedSpanExpr,
        TypedUnaryOp,
    },
};
use llamac_utils::{Ident, Span, Spanned, spanned};

use crate::{Constraint, Engine, InferError, InferResult};

impl Engine {
    /// Transform an expression into a typed expression, eagerly inferring the immediately obvious types, and generating type variables and constraints for the rest
    pub(super) fn infer_expr(
        &mut self,
        expr: SpanExpr,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        match *expr.node {
            Expr::Ident(var) => self.infer_ident(var, expr.span, expected),
            Expr::Literal(lit) => self.infer_literal(lit, expr.span, expected),
            Expr::List(list) => self.infer_list(list, expr.span, expected),
            Expr::UnaryOp(unary_op) => self.infer_unary_op(unary_op, expr.span, expected),
            Expr::BinaryOp(binary_op) => self.infer_binary_op(binary_op, expr.span, expected),
            Expr::FunCall(fun_call) => self.infer_fun_call(fun_call, expr.span, expected),
            Expr::Closure(closure) => self.infer_closure(closure, expr.span, expected),
            Expr::IfThen(if_then) => self.infer_if_then(if_then, expr.span, expected),
            Expr::Cond(cond) => self.infer_cond_expr(cond, expr.span, expected),
            Expr::Match(r#match) => self.infer_match_expr(r#match, expr.span, expected),
            Expr::Block(block) => self.infer_block_expr(block, expr.span, expected),
            Expr::Stmt(stmt) => self.infer_stmt_expr(stmt, expected),
        }
    }

    fn infer_ident(
        &mut self,
        var: Ident,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let ty = self.get_var(&var).ok_or_else(|| InferError::NotFound {
            ident: var.clone(),
            span,
        })?;
        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: ty.node.clone(),
            got_span: span,
        });
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::Ident(var),
                ty.node
            ))
        })
    }

    fn infer_literal(
        &mut self,
        lit: Literal,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let ty = match lit {
            Literal::Unit => Type::Unit,
            Literal::String(_) => Type::String,
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
        };
        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: ty.clone(),
            got_span: span,
        });
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::Literal(lit),
                ty,
            ))
        })
    }

    fn infer_list(
        &mut self,
        List(list): List,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let item_type = self.fresh_var();
        let new_items = list
            .into_iter()
            .map(|expr| -> InferResult<_> {
                self.infer_expr(expr, spanned! {expected.span, item_type.clone()})
            })
            .collect::<InferResult<Vec<_>>>()?;
        let list_type = Type::List(Box::new(item_type));
        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: list_type.clone(),
            got_span: span,
        });
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::List(TypedList(new_items)),
                list_type
            ))
        })
    }

    fn infer_unary_op(
        &mut self,
        UnaryOp { op, value }: UnaryOp,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let ty = match op.node {
            UnOp::Not => Type::Bool,
            UnOp::INegate => Type::Int,
            UnOp::FNegate => Type::Float,
        };
        let value_type = self.fresh_var();
        let new_value = self.infer_expr(value, spanned! {expected.span, value_type})?;
        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: ty.clone(),
            got_span: span,
        });
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::UnaryOp(TypedUnaryOp {
                    op,
                    value: new_value
                }),
                ty
            ))
        })
    }

    fn infer_binary_op(
        &mut self,
        BinaryOp { op, lhs, rhs }: BinaryOp,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;

        let (lhs_ty, rhs_ty, out_ty) = match op.node {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                (Type::Int, Type::Int, Type::Int)
            }
            BinOp::FAdd | BinOp::FSub | BinOp::FMul | BinOp::FDiv => {
                (Type::Float, Type::Float, Type::Float)
            }
            BinOp::And | BinOp::Or => (Type::Bool, Type::Bool, Type::Bool),
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                let (lhs_ty, rhs_ty) = (self.fresh_var(), self.fresh_var());
                (lhs_ty, rhs_ty, Type::Bool)
            }
            BinOp::Pipe => {
                let (arg, r#in, ret_ty) = (self.fresh_var(), self.fresh_var(), self.fresh_var());
                (
                    arg,
                    Type::Fun {
                        params: Types(vec![r#in]),
                        ret_ty: Box::new(ret_ty.clone()),
                    },
                    ret_ty,
                )
            }
            BinOp::Cons => {
                let (lhs_ty, rhs_ty) = (self.fresh_var(), self.fresh_var());
                (lhs_ty.clone(), rhs_ty, lhs_ty)
            }
        };

        let new_lhs = self.infer_expr(lhs, spanned! {expected.span, lhs_ty.clone()})?;
        let new_rhs = self.infer_expr(rhs, spanned! {expected.span, rhs_ty.clone()})?;

        self.constraints.push(match op.node {
            BinOp::Pipe => Constraint::Equality {
                expected: lhs_ty,
                expected_span: lhs_span,
                got: if let Type::Fun { params, .. } = rhs_ty {
                    params.0[0].clone()
                } else {
                    unreachable!()
                },
                got_span: rhs_span,
            },
            _ => Constraint::Equality {
                expected: lhs_ty,
                expected_span: lhs_span,
                got: rhs_ty,
                got_span: rhs_span,
            },
        });

        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: out_ty.clone(),
            got_span: span,
        });

        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::BinaryOp(TypedBinaryOp {
                    op,
                    lhs: new_lhs,
                    rhs: new_rhs,
                }),
                out_ty,
            ))
        })
    }

    fn infer_fun_call(
        &mut self,
        FunCall { fun, args }: FunCall,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let arg_tys = (0..args.node.0.len())
            .map(|_| self.fresh_var())
            .collect::<Vec<_>>();
        let fun_ty = self.fresh_var();
        let new_fun = self.infer_expr(fun, spanned! {0..0, fun_ty.clone()})?;
        let expected_fun_ty = Type::Fun {
            params: Types(arg_tys.clone()),
            ret_ty: Box::new(expected.node.clone()),
        };
        self.constraints.push(Constraint::Equality {
            expected: expected_fun_ty,
            expected_span: expected.span,
            got: fun_ty,
            got_span: span,
        });
        let new_args = args.map_res(|FunArgs(args)| -> InferResult<_> {
            Ok(TypedFunArgs(
                args.into_iter()
                    .zip(arg_tys)
                    .map(|(arg, ty)| -> InferResult<_> {
                        self.infer_expr(arg, spanned! {expected.span, ty})
                    })
                    .collect::<InferResult<Vec<_>>>()?,
            ))
        })?;
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::FunCall(TypedFunCall {
                    fun: new_fun,
                    args: new_args,
                }),
                expected.node
            ))
        })
    }

    fn infer_closure(
        &mut self,
        Closure {
            params,
            ret_ty,
            body,
        }: Closure,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let fake_span: Span = (params.span.end..params.span.end).into();
        let new_ret_ty = ret_ty
            .map(|ret_ty| ret_ty.map_ref(Type::from))
            .unwrap_or_else(|| spanned! {fake_span, self.fresh_var()});
        let new_param_tys = Types(
            params
                .node
                .0
                .clone()
                .into_iter()
                .map(|param| {
                    param
                        .node
                        .annot
                        .map(|ret_ty| (&ret_ty.node).into())
                        .unwrap_or_else(|| self.fresh_var())
                })
                .collect(),
        );
        let new_params = params.map(|params| {
            TypedClosureParams(
                params
                    .0
                    .into_iter()
                    .zip(new_param_tys.0.clone())
                    .map(|(param, ty)| {
                        spanned! {
                            param.span,
                            TypedClosureParam {
                                name: param.node.name,
                                annot: ty,
                            }
                        }
                    })
                    .collect(),
            )
        });
        self.enter_scope();
        for Spanned {
            node: TypedClosureParam { name, annot },
            ..
        } in new_params.node.0.iter()
        {
            self.extend(name.node.clone(), annot.clone(), name.span);
        }
        let new_body = self.infer_expr(body, new_ret_ty.clone())?;
        self.exit_scope();
        let fun_ty = Type::Fun {
            params: new_param_tys,
            ret_ty: Box::new(new_ret_ty.node.clone()),
        };
        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: fun_ty.clone(),
            got_span: span,
        });
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::Closure(TypedClosure {
                    params: new_params,
                    ret_ty: new_ret_ty,
                    body: new_body
                }),
                fun_ty
            ))
        })
    }

    fn infer_if_then(
        &mut self,
        IfThen { cond, then, r#else }: IfThen,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let new_cond = self.infer_expr(cond, spanned! {expected.span, Type::Bool})?;

        let then_ty = self.fresh_var();
        let new_then = self.infer_expr(then, spanned! {expected.span, then_ty.clone()})?;

        let r#else = r#else.ok_or(InferError::MissingElseBranch { span })?;
        let else_ty = self.fresh_var();
        let new_else = self.infer_expr(r#else, spanned! {expected.span, else_ty.clone()})?;

        self.constraints.push(Constraint::Equality {
            expected: then_ty.clone(),
            expected_span: new_then.span,
            got: else_ty,
            got_span: new_else.span,
        });
        self.constraints.push(Constraint::Equality {
            expected: expected.node.clone(),
            expected_span: expected.span,
            got: then_ty,
            got_span: span,
        });

        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::IfThen(TypedIfThenExpr {
                    cond: new_cond,
                    then: new_then,
                    r#else: new_else,
                }),
                expected.node,
            ))
        })
    }

    fn infer_cond_expr(
        &mut self,
        Cond { arms, r#else }: Cond,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let r#else = r#else.ok_or(InferError::MissingElseBranch { span })?;
        let else_ty = self.fresh_var();
        let new_else = self.infer_expr(r#else, spanned! {expected.span, else_ty.clone()})?;
        let new_arms = arms.map_res(|arms| {
            Ok(TypedCondArms(
                arms.0
                    .into_iter()
                    .map(|arm| {
                        arm.map_res(|arm| {
                            self.infer_cond_arm(arm, expected.span, &else_ty, new_else.span)
                        })
                    })
                    .collect::<InferResult<Vec<_>>>()?,
            ))
        })?;

        self.constraints.push(Constraint::Equality {
            expected: expected.node,
            expected_span: expected.span,
            got: else_ty.clone(),
            got_span: new_else.span,
        });
        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::Cond(TypedCondExpr {
                    arms: new_arms,
                    r#else: new_else,
                }),
                else_ty
            ))
        })
    }

    fn infer_cond_arm(
        &mut self,
        arm: CondArm,
        expected_span: Span,
        else_ty: &Type,
        new_else_span: Span,
    ) -> InferResult<TypedCondArm> {
        let new_cond = self.infer_expr(arm.cond, spanned! {expected_span, Type::Bool})?;
        let target_ty = self.fresh_var();
        let new_target =
            self.infer_expr(arm.target, spanned! {expected_span, target_ty.clone()})?;
        self.constraints.push(Constraint::Equality {
            expected: else_ty.clone(),
            expected_span: new_else_span,
            got: target_ty,
            got_span: new_target.span,
        });
        Ok(TypedCondArm {
            cond: new_cond,
            target: new_target,
        })
    }

    fn infer_match_expr(
        &mut self,
        r#match: Match,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        self.infer_match(r#match, span, expected.clone())
            .map(|typed_match| {
                typed_match.map(|typed_match| {
                    Box::new(TypedExpr(InnerExpr::Match(typed_match), expected.node))
                })
            })
    }

    // Does not perform exhaustivity checks on the match expression, this occurs after typechecking
    pub(super) fn infer_match(
        &mut self,
        Match { examinee, arms }: Match,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<Spanned<TypedMatch>> {
        let examinee_ty = self.fresh_var();
        let new_examinee =
            self.infer_expr(examinee, spanned! {expected.span, examinee_ty.clone()})?;
        let new_arms = arms.map_res(|MatchArms(arms)| -> InferResult<TypedMatchArms> {
            Ok(TypedMatchArms(
                arms.into_iter()
                    .map(|arm| {
                        arm.map_res(|arm| {
                            self.infer_match_arm(
                                arm,
                                expected.clone(),
                                &examinee_ty,
                                new_examinee.span,
                            )
                        })
                    })
                    .collect::<InferResult<Vec<_>>>()?,
            ))
        })?;
        Ok(spanned! {
            span,
            TypedMatch {
                examinee: new_examinee,
                arms: new_arms,
            }
        })
    }

    fn infer_match_arm(
        &mut self,
        arm: MatchArm,
        expected: Spanned<Type>,
        examinee_ty: &Type,
        examinee_span: Span,
    ) -> InferResult<TypedMatchArm> {
        for pattern in arm.patterns.node.0.iter() {
            self.enter_scope();
            let pattern_ty = match &pattern.node {
                MatchPattern::Wildcard | MatchPattern::NamedWildcard(_) => self.fresh_var(),
                MatchPattern::Literal(lit) => match lit {
                    Literal::Unit => Type::Unit,
                    Literal::String(_) => Type::String,
                    Literal::Int(_) => Type::Int,
                    Literal::Float(_) => Type::Float,
                    Literal::Bool(_) => Type::Bool,
                },
            };
            self.constraints.push(Constraint::Equality {
                expected: examinee_ty.clone(),
                expected_span: examinee_span,
                got: pattern_ty,
                got_span: pattern.span,
            });
        }
        let new_target = self.infer_expr(arm.target, expected)?;
        self.exit_scope();
        Ok(TypedMatchArm {
            patterns: arm.patterns,
            target: new_target,
        })
    }

    fn infer_block_expr(
        &mut self,
        Block { exprs, tail }: Block,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        self.enter_scope();
        let new_exprs = exprs
            .into_iter()
            .map(|expr| {
                let ty = self.fresh_var();
                self.infer_expr(expr, spanned! {span, ty})
            })
            .collect::<InferResult<Vec<_>>>()?;
        let new_tail = if let Some(tail) = tail {
            let tail_type = self.fresh_var();
            let tail_span = tail.span;
            let new_tail = self.infer_expr(tail, spanned! {span, tail_type.clone()})?;
            self.constraints.push(Constraint::Equality {
                expected: expected.node.clone(),
                expected_span: expected.span,
                got: tail_type,
                got_span: tail_span,
            });
            Some(new_tail)
        } else {
            self.constraints.push(Constraint::Equality {
                expected: expected.node.clone(),
                expected_span: expected.span,
                got: Type::Unit,
                got_span: span,
            });
            None
        };

        Ok(spanned! {
            span,
            Box::new(TypedExpr(
                InnerExpr::Block(TypedBlock {
                    exprs: new_exprs,
                    tail: new_tail,
                }),
                expected.node
            ))
        })
    }

    fn infer_stmt_expr(
        &mut self,
        stmt: SpanStmt,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let stmt_span = stmt.span;
        let new_stmt = self.infer_stmt(stmt)?;
        self.constraints.push(Constraint::Equality {
            expected: expected.node.clone(),
            expected_span: expected.span,
            got: Type::Unit,
            got_span: stmt_span,
        });
        Ok(spanned! {
            stmt_span,
            Box::new(TypedExpr(
                InnerExpr::Stmt(new_stmt),
                expected.node,
            ))
        })
    }
}
