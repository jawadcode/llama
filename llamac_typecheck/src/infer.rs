use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use llamac_ast::{
    expr::{BinOp, Expr, ListIndex, Literal, SpanExpr, UnOp},
    spanned,
    utils::Span,
    Ident,
};

use crate::{
    ty::Type,
    typed_ast::expr::{InnerExpr, TypedExpr, TypedList, TypedListIndex, TypedUnaryOp},
};

/// The relationship between 2 types, can be used to constrain type variables to more concrete types
enum Constraint {
    /// An equality constraint
    Eq(Type, Type),
}

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Eq(type0, type1) => write!(f, "{type0} == {type1}"),
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
    pub fn infer(&mut self, expected: Type, expr: SpanExpr) -> InferResult<TypedExpr> {
        match expr.node.as_ref() {
            Expr::Ident(ident) => {
                let ty = self.get_var(&ident, expr.span)?;
                self.constraints.push(Constraint::Eq(expected, ty.clone()));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::Ident(ident.clone()))
                    },
                    ty,
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
                self.constraints.push(Constraint::Eq(expected, ty.clone()));
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::Literal(literal.clone()))
                    },
                    ty,
                ))
            }
            Expr::List(list) => {
                let item_ty = self.fresh_typevar();
                let mut new_items = Vec::with_capacity(list.0.len());
                for item in list
                    .0
                    .clone()
                    .into_iter()
                    .map(|item| self.infer(item_ty.clone(), item))
                {
                    new_items.push(item?);
                }
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::List(TypedList(new_items)))
                    },
                    Type::List(Box::new(item_ty)),
                ))
            }
            Expr::ListIndex(index) => {
                let new_index = self.infer(Type::Int, index.index.clone())?;
                let item_ty = self.fresh_typevar();
                let new_list =
                    self.infer(Type::List(Box::new(item_ty.clone())), index.list.clone())?;
                Ok(TypedExpr(
                    spanned! {
                        expr.span,
                        Box::new(InnerExpr::ListIndex(TypedListIndex {
                            list: new_list,
                            index: new_index,
                        }))
                    },
                    item_ty,
                ))
            }
            Expr::UnaryOp(unary_op) => {
                let value_ty = match unary_op.op {
                    UnOp::Not => Type::Bool,
                    UnOp::Negate => Type::Int,
                };
                let new_value = self.infer(value_ty.clone(), unary_op.value.clone())?;
                Ok(TypedExpr(
                    spanned! {
                    expr.span,
                    Box::new(InnerExpr::UnaryOp(TypedUnaryOp {
                        op: unary_op.op.clone(),
                        value: new_value
                    }))},
                    value_ty,
                ))
            }
            Expr::BinaryOp(binary_op) => {
                let value_ty = match binary_op.op {
                    BinOp::Add => todo!(),
                    BinOp::Sub => todo!(),
                    BinOp::Mul => todo!(),
                    BinOp::Div => todo!(),
                    BinOp::Mod => todo!(),
                    BinOp::And => todo!(),
                    BinOp::Or => todo!(),
                    BinOp::Xor => todo!(),
                    BinOp::Lt => todo!(),
                    BinOp::Leq => todo!(),
                    BinOp::Gt => todo!(),
                    BinOp::Geq => todo!(),
                    BinOp::Eq => todo!(),
                    BinOp::Neq => todo!(),
                    BinOp::Pipe => todo!(),
                    BinOp::Concat => todo!(),
                };
                todo!()
            }
            Expr::FunCall(_) => todo!(),
            Expr::Closure(_) => todo!(),
            Expr::IfThen(_) => todo!(),
            Expr::Cond(_) => todo!(),
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
