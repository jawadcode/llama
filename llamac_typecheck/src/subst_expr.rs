use llamac_typed_ast::expr::{
    InnerExpr, TypedBinaryOp, TypedBlock, TypedClosure, TypedClosureParam, TypedClosureParams,
    TypedCondArm, TypedCondArms, TypedCondExpr, TypedExpr, TypedFunArgs, TypedFunCall,
    TypedIfThenExpr, TypedList, TypedMatch, TypedMatchArm, TypedMatchArms, TypedSpanExpr,
    TypedUnaryOp,
};

use super::Engine;

impl Engine {
    pub(super) fn subst_expr(&self, expr: TypedSpanExpr) -> TypedSpanExpr {
        expr.map(|expr| {
            let TypedExpr(expr, ty) = *expr;
            let expr = match expr {
                e @ InnerExpr::Ident(_) | e @ InnerExpr::Literal(_) => e,
                InnerExpr::List(TypedList(list)) => InnerExpr::List(TypedList(
                    list.into_iter().map(|item| self.subst_expr(item)).collect(),
                )),
                InnerExpr::UnaryOp(unary_op) => self.subst_unary_op(unary_op),
                InnerExpr::BinaryOp(binary_op) => self.subst_binary_op(binary_op),
                InnerExpr::FunCall(fun_call) => self.subst_fun_call(fun_call),
                InnerExpr::Closure(closure) => self.subst_closure(closure),
                InnerExpr::IfThen(if_then) => self.subst_if_then_expr(if_then),
                InnerExpr::Cond(cond) => self.subst_cond_expr(cond),
                InnerExpr::Match(r#match) => self.subst_match_expr(r#match),
                InnerExpr::Block(block) => self.subst_block(block),
                InnerExpr::Stmt(stmt) => InnerExpr::Stmt(self.subst_stmt(stmt)),
            };
            Box::new(TypedExpr(expr, self.subst_ty(ty)))
        })
    }

    fn subst_unary_op(&self, TypedUnaryOp { op, value }: TypedUnaryOp) -> InnerExpr {
        InnerExpr::UnaryOp(TypedUnaryOp {
            op,
            value: self.subst_expr(value),
        })
    }

    fn subst_binary_op(&self, TypedBinaryOp { op, lhs, rhs }: TypedBinaryOp) -> InnerExpr {
        InnerExpr::BinaryOp(TypedBinaryOp {
            op,
            lhs: self.subst_expr(lhs),
            rhs: self.subst_expr(rhs),
        })
    }

    fn subst_fun_call(&self, TypedFunCall { fun, args }: TypedFunCall) -> InnerExpr {
        InnerExpr::FunCall(TypedFunCall {
            fun: self.subst_expr(fun),
            args: args.map(|TypedFunArgs(args)| {
                TypedFunArgs(args.into_iter().map(|arg| self.subst_expr(arg)).collect())
            }),
        })
    }

    fn subst_closure(
        &self,
        TypedClosure {
            params,
            ret_ty,
            body,
        }: TypedClosure,
    ) -> InnerExpr {
        InnerExpr::Closure(TypedClosure {
            params: params.map(|TypedClosureParams(params)| {
                TypedClosureParams(
                    params
                        .into_iter()
                        .map(|param| {
                            param.map(|param| TypedClosureParam {
                                name: param.name,
                                annot: self.subst_ty(param.annot),
                            })
                        })
                        .collect(),
                )
            }),
            ret_ty: ret_ty.map(|ret_ty| self.subst_ty(ret_ty)),
            body: self.subst_expr(body),
        })
    }

    fn subst_if_then_expr(
        &self,
        TypedIfThenExpr { cond, then, r#else }: TypedIfThenExpr,
    ) -> InnerExpr {
        InnerExpr::IfThen(TypedIfThenExpr {
            cond: self.subst_expr(cond),
            then: self.subst_expr(then),
            r#else: self.subst_expr(r#else),
        })
    }

    fn subst_cond_expr(&self, TypedCondExpr { arms, r#else }: TypedCondExpr) -> InnerExpr {
        InnerExpr::Cond(TypedCondExpr {
            arms: arms.map(|TypedCondArms(arms)| {
                TypedCondArms(
                    arms.into_iter()
                        .map(|arm| {
                            arm.map(|arm| TypedCondArm {
                                cond: self.subst_expr(arm.cond),
                                target: self.subst_expr(arm.target),
                            })
                        })
                        .collect(),
                )
            }),
            r#else: self.subst_expr(r#else),
        })
    }

    fn subst_match_expr(&self, TypedMatch { examinee, arms }: TypedMatch) -> InnerExpr {
        InnerExpr::Match(TypedMatch {
            examinee: self.subst_expr(examinee),
            arms: arms.map(|TypedMatchArms(arms)| {
                TypedMatchArms(
                    arms.into_iter()
                        .map(|arm| {
                            arm.map(|arm| TypedMatchArm {
                                patterns: arm.patterns,
                                target: self.subst_expr(arm.target),
                            })
                        })
                        .collect(),
                )
            }),
        })
    }

    fn subst_block(&self, TypedBlock { exprs, tail }: TypedBlock) -> InnerExpr {
        InnerExpr::Block(TypedBlock {
            exprs: exprs
                .into_iter()
                .map(|expr| self.subst_expr(expr))
                .collect(),
            tail: tail.map(|expr| self.subst_expr(expr)),
        })
    }
}
