use llamac_ast::stmt::{Const, FunDef, FunParam};
use llamac_typed_ast::{
    stmt::{TypedConst, TypedFunDef, TypedFunParam, TypedFunParams},
    Type, Types,
};
use llamac_utils::Spanned;

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
                params: params.clone().map(|params| {
                    Types(
                        params
                            .0
                            .into_iter()
                            .map(
                                |Spanned {
                                     span: _,
                                     node: TypedFunParam { name: _, annot },
                                 }| annot,
                            )
                            .collect(),
                    )
                }),
                ret_ty: ret_ty.clone().map(Box::new),
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
}
