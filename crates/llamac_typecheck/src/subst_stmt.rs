use llamac_typed_ast::{
    expr::{TypedCondArm, TypedCondArms, TypedMatch, TypedMatchArm, TypedMatchArms},
    stmt::{
        TypedCondStmt, TypedConst, TypedFunDef, TypedFunParam, TypedFunParams, TypedIfThenStmt,
        TypedLetBind, TypedSpanStmt, TypedStmt,
    },
};

use super::Engine;

impl Engine {
    pub(super) fn subst_stmt(&self, stmt: TypedSpanStmt) -> TypedSpanStmt {
        stmt.map(|stmt| match stmt {
            TypedStmt::Const(r#const) => TypedStmt::Const(self.subst_const(r#const)),
            TypedStmt::LetBind(let_bind) => TypedStmt::LetBind(self.subst_let_bind(let_bind)),
            TypedStmt::FunDef(fun_def) => TypedStmt::FunDef(self.subst_fun_def(fun_def)),
            TypedStmt::IfThen(if_then) => TypedStmt::IfThen(self.subst_if_then_stmt(if_then)),
            TypedStmt::Cond(cond) => TypedStmt::Cond(self.subst_cond_stmt(cond)),
            TypedStmt::Match(r#match) => TypedStmt::Match(self.subst_match_stmt(r#match)),
        })
    }

    pub(super) fn subst_const(&self, TypedConst { name, annot, value }: TypedConst) -> TypedConst {
        TypedConst {
            name,
            annot: annot.map(|ty| self.subst_ty(ty)),
            value: self.subst_expr(value),
        }
    }

    fn subst_let_bind(&self, TypedLetBind { name, annot, value }: TypedLetBind) -> TypedLetBind {
        TypedLetBind {
            name,
            annot: annot.map(|ty| self.subst_ty(ty)),
            value: self.subst_expr(value),
        }
    }

    pub(super) fn subst_fun_def(
        &self,
        TypedFunDef {
            name,
            params,
            ret_ty,
            body,
        }: TypedFunDef,
    ) -> TypedFunDef {
        TypedFunDef {
            name,
            params: params.map(|TypedFunParams(params)| {
                TypedFunParams(
                    params
                        .into_iter()
                        .map(|param| {
                            param.map(|TypedFunParam { name, annot }| TypedFunParam {
                                name,
                                annot: annot.map(|ty| self.subst_ty(ty)),
                            })
                        })
                        .collect(),
                )
            }),
            ret_ty: ret_ty.map(|ty| self.subst_ty(ty)),
            body: self.subst_expr(body),
        }
    }

    fn subst_if_then_stmt(
        &self,
        TypedIfThenStmt { cond, then, r#else }: TypedIfThenStmt,
    ) -> TypedIfThenStmt {
        TypedIfThenStmt {
            cond: self.subst_expr(cond),
            then: self.subst_expr(then),
            r#else: r#else.map(|r#else| self.subst_expr(r#else)),
        }
    }

    fn subst_cond_stmt(&self, TypedCondStmt { arms, r#else }: TypedCondStmt) -> TypedCondStmt {
        TypedCondStmt {
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
            r#else: r#else.map(|r#else| self.subst_expr(r#else)),
        }
    }

    fn subst_match_stmt(&self, TypedMatch { examinee, arms }: TypedMatch) -> TypedMatch {
        TypedMatch {
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
        }
    }
}
