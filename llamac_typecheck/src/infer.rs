use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use llamac_ast::{
    expr::{
        BinOp, BinaryOp, Closure, Cond, Expr, FunCall, IfThen, List, ListIndex, Literal, SpanExpr,
        UnOp, UnaryOp,
    },
    spanned,
    stmt::{Const, FunDef, LetBind, SpanStmt, Stmt},
    utils::{Span, Spanned},
    Ident,
};

use crate::{
    ty::Type,
    typed_ast::{
        expr::{
            InnerExpr, TypedBinaryOp, TypedClosure, TypedClosureParam, TypedClosureParams,
            TypedCondArm, TypedExpr, TypedFunArgs, TypedFunCall, TypedIfThen, TypedList,
            TypedListIndex, TypedUnaryOp,
        },
        stmt::{InnerStmt, TypedConst, TypedLetBind, TypedStmt},
    },
};

/// The relationship between 2 types, can be used to constrain type variables to more concrete types
enum Constraint {
    /// An equality constraint
    Equality(Type, Type),
    // The relationship between types in an arithmetic expression and concatenation
    Triple {
        lhs: Type,
        rhs: Type,
        out: Type,
    },
}

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Equality(type0, type1) => write!(f, "{type0} == {type1}"),
            Constraint::Triple { lhs, rhs, out } => write!(f, "{lhs} ⨁ {rhs} = {out}"),
        }
    }
}

/// The type inference engine
pub struct Engine {
    /// The full list of constraints to be applied to an expression
    constraints: Vec<Constraint>,
    /// A set of mappings from type variables (represented by `usize`) to `Type`s
    subst: Vec<Type>,
    /// A stack of scopes
    env: Vec<HashMap<Ident, Type>>,
}

pub enum TypeError {
    NotInScope(Ident, Span),
}

pub type InferResult<T> = Result<T, TypeError>;

impl Engine {
    fn infer_stmt(&mut self, stmt: SpanStmt) -> InferResult<TypedStmt> {
        match &stmt.node {
            Stmt::Const(Const { name, annot, value }) => {
                let ty: Type = (&annot.node).into();
                let new_value = self.infer_expr(ty.clone(), value.clone())?;
                self.insert_var(name.clone().node, ty.clone());
                Ok(spanned! {
                    stmt.span,
                    InnerStmt::Const(TypedConst {
                        name: name.clone(),
                        annot: spanned! {annot.span, ty},
                        value: new_value
                    })
                })
            }
            Stmt::LetBind(LetBind { name, annot, value }) => {
                let ty = match annot {
                    Some(ty_expr) => spanned! {ty_expr.span, (&ty_expr.node).into()},
                    None => spanned! {name.span.end..name.span.end, self.fresh_typevar()},
                };
                let new_value = self.infer_expr(ty.node.clone(), value.clone())?;
                self.insert_var(name.clone().node, ty.node.clone());
                Ok(spanned! {
                    stmt.span,
                    InnerStmt::LetBind(TypedLetBind {
                        name: name.clone(),
                        annot: ty,
                        value: new_value
                    })
                })
            }
            Stmt::FunDef(FunDef {
                name,
                params,
                ret_ty,
                body,
            }) => {
                todo!()
            }
            Stmt::IfThen(_) => todo!(),
            Stmt::Cond(_) => todo!(),
            Stmt::Match(_) => todo!(),
        }
    }

    fn infer_expr(&mut self, expected: Type, expr: SpanExpr) -> InferResult<TypedExpr> {
        match expr.node.as_ref() {
            Expr::Ident(ident) => {
                let ty = self.get_var(&ident, expr.span)?;
                self.constraints
                    .push(Constraint::Equality(expected.clone(), ty.clone()));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::Ident(ident.clone()))
                    },
                    expected,
                ))
            }
            Expr::Literal(literal) => {
                let ty = match literal {
                    Literal::Unit => Type::Unit,
                    Literal::String(_) => Type::String,
                    Literal::Int(_) => Type::Int,
                    Literal::Float(_) => Type::Float,
                    Literal::Bool(_) => Type::Bool,
                };
                self.constraints
                    .push(Constraint::Equality(expected.clone(), ty.clone()));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::Literal(literal.clone()))
                    },
                    expected,
                ))
            }
            Expr::List(List(list)) => {
                let item_ty = self.fresh_typevar();
                let mut new_items = Vec::with_capacity(list.len());
                for item in list
                    .clone()
                    .into_iter()
                    .map(|item| self.infer_expr(item_ty.clone(), item))
                {
                    new_items.push(item?);
                }
                self.constraints.push(Constraint::Equality(
                    expected.clone(),
                    Type::List(Box::new(item_ty)),
                ));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::List(TypedList(new_items)))
                    },
                    expected,
                ))
            }
            Expr::ListIndex(ListIndex { list, index }) => {
                let new_index = self.infer_expr(Type::Int, index.clone())?;
                let list_ty = Type::List(Box::new(self.fresh_typevar()));
                let new_list = self.infer_expr(list_ty.clone(), list.clone())?;
                self.constraints
                    .push(Constraint::Equality(expected.clone(), list_ty));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::ListIndex(TypedListIndex {
                            list: new_list,
                            index: new_index,
                        }))
                    },
                    expected,
                ))
            }
            Expr::UnaryOp(UnaryOp { op, value }) => {
                let value_ty = match op {
                    UnOp::Not => Type::Bool,
                    UnOp::Negate => Type::Int,
                };
                let new_value = self.infer_expr(value_ty.clone(), value.clone())?;
                self.constraints
                    .push(Constraint::Equality(expected.clone(), value_ty));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::UnaryOp(TypedUnaryOp {
                            op: op.clone(),
                            value: new_value
                        }))
                    },
                    expected,
                ))
            }
            Expr::BinaryOp(BinaryOp { op, lhs, rhs }) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    let new_lhs = self.infer_expr(Type::Number, lhs.clone())?;
                    let new_rhs = self.infer_expr(Type::Number, rhs.clone())?;
                    self.constraints
                        .push(Constraint::Equality(expected.clone(), Type::Number));
                    self.constraints.push(Constraint::Triple {
                        lhs: new_lhs.1.clone(),
                        rhs: new_rhs.1.clone(),
                        out: expected.clone(),
                    });
                    Ok(TypedExpr(
                        spanned! {
                            expr.span,
                            Box::new(InnerExpr::BinaryOp(TypedBinaryOp {
                                op: op.clone(),
                                lhs: new_lhs,
                                rhs: new_rhs,
                            }))
                        },
                        expected,
                    ))
                }
                BinOp::Mod => {
                    let new_lhs = self.infer_expr(Type::Int, lhs.clone())?;
                    let new_rhs = self.infer_expr(Type::Int, rhs.clone())?;
                    self.constraints
                        .push(Constraint::Equality(expected.clone(), Type::Int));
                    Ok(TypedExpr(
                        spanned! {
                            expr.span,
                            Box::new(InnerExpr::BinaryOp(TypedBinaryOp {
                                op: op.clone(),
                                lhs: new_lhs,
                                rhs: new_rhs
                            }))
                        },
                        expected,
                    ))
                }
                BinOp::And | BinOp::Or | BinOp::Xor => {
                    let new_lhs = self.infer_expr(Type::Bool, lhs.clone())?;
                    let new_rhs = self.infer_expr(Type::Bool, rhs.clone())?;
                    self.constraints
                        .push(Constraint::Equality(expected.clone(), Type::Bool));
                    Ok(TypedExpr(
                        spanned! {
                            expr.span,
                            Box::new(InnerExpr::BinaryOp(TypedBinaryOp {
                                op: op.clone(),
                                lhs: new_lhs,
                                rhs: new_rhs,
                            }))
                        },
                        expected,
                    ))
                }
                BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq | BinOp::Eq | BinOp::Neq => {
                    let new_lhs = self.infer_expr(Type::Comparable, lhs.clone())?;
                    let new_rhs = self.infer_expr(Type::Comparable, rhs.clone())?;
                    self.constraints
                        .push(Constraint::Equality(new_lhs.1.clone(), new_rhs.1.clone()));
                    self.constraints
                        .push(Constraint::Equality(expected.clone(), Type::Bool));
                    Ok(TypedExpr(
                        spanned! {
                            expr.span,
                            Box::new(InnerExpr::BinaryOp(TypedBinaryOp {
                                op: op.clone(),
                                lhs: new_lhs,
                                rhs: new_rhs,
                            }))
                        },
                        expected,
                    ))
                }
                BinOp::Pipe => {
                    todo!("Couldn't be bothered implementing type inference for function pipes lol")
                }
                BinOp::Concat => {
                    let new_lhs = self.infer_expr(Type::Concatenable, lhs.clone())?;
                    let new_rhs = self.infer_expr(Type::Concatenable, rhs.clone())?;
                    self.constraints
                        .push(Constraint::Equality(expected.clone(), Type::Concatenable));
                    self.constraints.push(Constraint::Triple {
                        lhs: new_lhs.1.clone(),
                        rhs: new_rhs.1.clone(),
                        out: expected.clone(),
                    });
                    Ok(TypedExpr(
                        spanned! {
                            expr.span,
                            Box::new(InnerExpr::BinaryOp(TypedBinaryOp {
                                op: op.clone(),
                                lhs: new_lhs,
                                rhs: new_rhs,
                            }))
                        },
                        expected,
                    ))
                }
            },
            Expr::FunCall(FunCall { fun, args }) => {
                let args_tys: Vec<Type> =
                    args.node.0.iter().map(|_| self.fresh_typevar()).collect();
                let fun_ty = Type::Fun {
                    params: args_tys.clone(),
                    ret_ty: Box::new(expected.clone()),
                };
                let new_fun = self.infer_expr(fun_ty, fun.clone())?;
                let new_args_iter = args.node.0.iter().zip(&args_tys);
                let mut new_args = Vec::new();
                for (expr, ty) in new_args_iter {
                    new_args.push(self.infer_expr(ty.clone(), expr.clone())?);
                }
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::FunCall(TypedFunCall {
                            fun: new_fun,
                            args: spanned! {args.span, TypedFunArgs(new_args)},
                        }))
                    },
                    expected,
                ))
            }
            Expr::Closure(Closure {
                params,
                ret_ty,
                body,
            }) => {
                let new_param_tys: Vec<Type> = params
                    .node
                    .0
                    .iter()
                    .map(|param| match param.node.annot.clone() {
                        Some(ty_expr) => ty_expr.map(|ty_expr| (&ty_expr).into()).node,
                        None => self.fresh_typevar(),
                    })
                    .collect();
                let new_params = params.clone().map(|params| {
                    TypedClosureParams(
                        params
                            .0
                            .into_iter()
                            .zip(&new_param_tys)
                            .map(|(param, ty)| TypedClosureParam {
                                name: param.node.name,
                                ty: ty.clone(),
                            })
                            .collect(),
                    )
                });
                let new_ret_ty = match ret_ty {
                    Some(ret_ty) => ret_ty.clone().map(|ty_expr| (&ty_expr).into()),
                    None => spanned! {params.span.end..params.span.end, self.fresh_typevar()},
                };
                self.enter_scope();
                for TypedClosureParam { name, ty } in new_params.node.0.clone() {
                    self.insert_var(name.node, ty)
                }
                let new_body = self.infer_expr(new_ret_ty.node.clone(), body.clone())?;
                self.constraints.push(Constraint::Equality(
                    expected.clone(),
                    Type::Fun {
                        params: new_param_tys,
                        ret_ty: Box::new(new_ret_ty.node.clone()),
                    },
                ));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::Closure(TypedClosure {
                            params: new_params,
                            ret_ty: new_ret_ty,
                            body: new_body,
                        }))
                    },
                    expected,
                ))
            }
            Expr::IfThen(IfThen { cond, then, r#else }) => {
                let new_cond = self.infer_expr(Type::Bool, cond.clone())?;
                let then_ty = self.fresh_typevar();
                let new_then = self.infer_expr(then_ty.clone(), then.clone())?;
                let else_ty = self.fresh_typevar();
                let new_else = match r#else {
                    Some(r#else) => Some(self.infer_expr(else_ty.clone(), r#else.clone())?),
                    None => None,
                };
                self.constraints
                    .push(Constraint::Equality(then_ty, else_ty));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::IfThen(TypedIfThen {
                            cond: new_cond,
                            then: new_then,
                            r#else: new_else,
                        }))
                    },
                    expected,
                ))
            }
            Expr::Cond(Cond { arms, r#else }) => {
                let mut new_arms = Vec::new();
                for arm in arms.node.0.clone() {
                    let new_cond = self.infer_expr(Type::Bool, arm.node.cond)?;
                    let branch_ty = self.fresh_typevar();
                    let new_branch = self.infer_expr(branch_ty.clone(), arm.node.branch)?;
                    new_arms.push(TypedCondArm {
                        cond: new_cond,
                        branch: new_branch,
                    });
                }
                for arm in new_arms {}
                todo!()
            }
            Expr::Match(_) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::Stmt(_) => todo!(),
        }
    }

    fn get_var(&mut self, ident: &Ident, span: Span) -> InferResult<Type> {
        for scope in self.env.iter().rev() {
            match scope.get(ident) {
                Some(ty) => return Ok(ty.clone()),
                None => (),
            }
        }
        Err(TypeError::NotInScope(ident.clone(), span))
    }

    fn insert_var(&mut self, ident: Ident, ty: Type) {
        self.env.last_mut().unwrap().insert(ident, ty);
    }

    fn enter_scope(&mut self) {
        self.env.push(HashMap::new())
    }

    fn exit_scope(&mut self) {
        self.env.pop();
    }

    fn fresh_typevar(&mut self) -> Type {
        let result = Type::TypeVar(self.subst.len());
        self.subst.push(result.clone());
        result
    }
}
