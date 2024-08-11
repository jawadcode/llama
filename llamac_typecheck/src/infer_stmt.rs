use llamac_ast::expr::Match;
use llamac_ast::{
    expr::{Cond, CondArm, CondArms, IfThen},
    stmt::{Const, FunDef, FunParam, LetBind, SpanStmt, Stmt},
};

use llamac_typed_ast::{
    expr::{TypedCondArm, TypedCondArms},
    stmt::{
        TypedCondStmt, TypedConst, TypedFunDef, TypedFunParam, TypedFunParams, TypedIfThenStmt,
        TypedLetBind, TypedSpanStmt, TypedStmt,
    },
    Type, Types,
};
use llamac_utils::{spanned, Span, Spanned};

use crate::{Engine, InferResult};

impl Engine {
    pub(super) fn infer_stmt(&mut self, stmt: SpanStmt) -> InferResult<TypedSpanStmt> {
        match stmt.node {
            Stmt::Const(r#const) => self
                .infer_const(r#const, false)
                .map(|r#const| spanned! { stmt.span, TypedStmt::Const(r#const) }),
            Stmt::LetBind(let_bind) => self.infer_let_bind(let_bind, stmt.span),
            Stmt::FunDef(fun_def) => self
                .infer_fun_def(fun_def, false)
                .map(|fun_def| spanned! { stmt.span, TypedStmt::FunDef(fun_def) }),
            Stmt::IfThen(if_then) => self.infer_if_stmt(if_then, stmt.span),
            Stmt::Cond(cond) => self.infer_cond_stmt(cond, stmt.span),
            Stmt::Match(r#match) => self.infer_match_stmt(r#match, stmt.span),
        }
    }

    /// Typecheck a constant declaration (if top_level then the name and type will be added to the scope)
    pub(super) fn infer_const(
        &mut self,
        Const { name, annot, value }: Const,
        top_level: bool,
    ) -> InferResult<TypedConst> {
        let annot = annot.map_ref(Type::from);
        let value = self.infer_expr(value, annot.clone())?;
        if !top_level {
            self.extend(name.node.clone(), annot.node.clone(), annot.span);
        }
        Ok(TypedConst { name, annot, value })
    }

    /// Typecheck a function definition (if not top_level then the name and type will be added to the scope)
    pub(super) fn infer_fun_def(
        &mut self,
        FunDef {
            name,
            params,
            ret_ty,
            body,
        }: FunDef,
        top_level: bool,
    ) -> InferResult<TypedFunDef> {
        let params = params.map(|params| {
            TypedFunParams(
                params
                    .0
                    .into_iter()
                    .map(|param| {
                        param.map(|FunParam { name, annot }| TypedFunParam {
                            name,
                            annot: annot.map_ref(Type::from),
                        })
                    })
                    .collect(),
            )
        });

        // Create a new scope, adding all of the function parameters to it
        self.enter_scope();
        for Spanned {
            span,
            node: TypedFunParam { name, annot },
        } in params.node.0.iter()
        {
            self.extend(name.node.clone(), annot.node.clone(), *span);
        }
        let ret_ty: Spanned<Type> = ret_ty.map_ref(Type::from);
        let body = self.infer_expr(body, ret_ty.clone())?;
        self.exit_scope();
        if !top_level {
            let ty = Type::Fun {
                params: Types(
                    params
                        .node
                        .0
                        .iter()
                        .map(
                            |Spanned {
                                 span: _,
                                 node: TypedFunParam { name: _, annot },
                             }| annot.node.clone(),
                        )
                        .collect(),
                ),
                ret_ty: Box::new(ret_ty.node.clone()),
            };
            self.extend(name.node.clone(), ty, name.span);
        }

        Ok(TypedFunDef {
            name,
            params,
            ret_ty,
            body,
        })
    }

    pub(super) fn infer_let_bind(
        &mut self,
        LetBind { name, annot, value }: LetBind,
        span: Span,
    ) -> InferResult<TypedSpanStmt> {
        let annot = match annot {
            Some(annot) => annot.map_ref(Type::from),
            None => spanned! { span, self.fresh_var() },
        };
        let new_value = self.infer_expr(value, annot.clone())?;
        self.extend(name.node.clone(), annot.node.clone(), annot.span);
        Ok(spanned! {
            span,
            TypedStmt::LetBind(TypedLetBind {
                name,
                annot,
                value: new_value,
            })
        })
    }

    pub(super) fn infer_if_stmt(
        &mut self,
        IfThen { cond, then, r#else }: IfThen,
        span: Span,
    ) -> InferResult<TypedSpanStmt> {
        let new_cond = self.infer_expr(cond, spanned! {span, Type::Bool})?;
        let new_then = self.infer_expr(then, spanned! {span, Type::Unit})?;
        let new_else = if let Some(r#else) = r#else {
            Some(self.infer_expr(r#else, spanned! {span, Type::Unit})?)
        } else {
            None
        };
        Ok(spanned! {
            span,
            TypedStmt::IfThen(TypedIfThenStmt {
                cond: new_cond,
                then: new_then,
                r#else: new_else,
            })
        })
    }

    // Ideally much of this could be deduplicated but the optionality of the else clause when a
    // cond is in statement position poses an issue, as well as the bounds on `Spanned`
    fn infer_cond_stmt(
        &mut self,
        Cond { arms, r#else }: Cond,
        span: Span,
    ) -> InferResult<TypedSpanStmt> {
        let new_arms = arms.map_res(|CondArms(arms)| {
            Ok(TypedCondArms(
                arms.into_iter()
                    .map(|arm| {
                        arm.map_res(|CondArm { cond, target }| {
                            let new_cond = self.infer_expr(cond, spanned! { span, Type::Bool })?;
                            let new_target =
                                self.infer_expr(target, spanned! { span, Type::Unit })?;
                            Ok(TypedCondArm {
                                cond: new_cond,
                                target: new_target,
                            })
                        })
                    })
                    .collect::<InferResult<_>>()?,
            ))
        })?;

        let new_else = r#else
            .map(|r#else| self.infer_expr(r#else, spanned! { span, Type::Unit }))
            .transpose()?;

        Ok(spanned! {
            span,
            TypedStmt::Cond(TypedCondStmt {
                arms: new_arms,
                r#else: new_else,
            })
        })
    }

    fn infer_match_stmt(&mut self, r#match: Match, span: Span) -> InferResult<TypedSpanStmt> {
        self.infer_match(r#match, span, spanned! { span, Type::Unit })
            .map(|typed_match| typed_match.map(TypedStmt::Match))
    }
}
