use llamac_ast::{
    expr::IfThen,
    stmt::{Const, FunDef, FunParam, LetBind},
};
use llamac_typed_ast::{
    stmt::{
        TypedConst, TypedFunDef, TypedFunParam, TypedFunParams, TypedIfThenStmt, TypedSpanStmt,
        TypedStmt,
    },
    Type, Types,
};
use llamac_utils::{spanned, Span, Spanned};

use crate::{Engine, InferResult};

impl Engine {
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

    /// Typecheck a function definition (if top_level then the name and type will be added to the scope)
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
        let body = self.infer_expr(body, ret_ty.clone())?;
        self.exit_scope();

        Ok(TypedFunDef {
            name,
            params,
            ret_ty,
            body,
        })
    }

    fn infer_let_bind(
        &mut self,
        LetBind { name, annot, value }: LetBind,
    ) -> InferResult<TypedSpanStmt> {
        todo!()
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
}
