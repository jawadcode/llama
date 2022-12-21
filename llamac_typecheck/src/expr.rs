use llamac_ast::expr::{Expr, List, ListIndex, Literal, SpanExpr};
use llamac_typed_ast::{
    expr::{InnerExpr, TypedExpr, TypedList, TypedSpanExpr},
    Type,
};
use llamac_utils::{spanned, Ident, Span, Spanned};

use crate::{Constraint, Engine, InferError, InferResult};

impl Engine {
    /// Transform an expression into a typed expression, eagerly inferring the immediately obvious types, and generating a type variable and constraints for the rest
    pub(super) fn infer_expr(
        &mut self,
        expr: SpanExpr,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        match *expr.node {
            Expr::Ident(var) => self.infer_ident(var, expr.span, expected),
            Expr::Literal(lit) => self.infer_literal(lit, expr.span, expected),
            Expr::List(list) => self.infer_list(list, expr.span, expected),
            Expr::ListIndex(list_index) => self.infer_list_index(list_index, expr.span, expected),
            Expr::UnaryOp(_) => todo!(),
            Expr::BinaryOp(_) => todo!(),
            Expr::FunCall(_) => todo!(),
            Expr::Closure(_) => todo!(),
            Expr::IfThen(_) => todo!(),
            Expr::Cond(_) => todo!(),
            Expr::Match(_) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::Stmt(_) => todo!(),
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
                InnerExpr::Literal(lit.clone()),
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
        let mut new_items = Vec::with_capacity(list.len());
        for expr in list {
            let new_item = self.infer_expr(expr, spanned! {expected.span, item_type.clone()})?;
            new_items.push(new_item);
        }
        let list_type = Type::List(spanned! {expected.span, Box::new(item_type.clone())});
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

    fn infer_list_index(
        &mut self,
        ListIndex { list, index }: ListIndex,
        span: Span,
        expected: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        let list_type = self.fresh_var();
        let new_list = self.infer_expr(list, spanned! {expected.span, list_type});
        todo!("")
    }
}
