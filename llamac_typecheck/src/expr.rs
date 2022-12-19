use llamac_ast::expr::SpanExpr;
use llamac_typed_ast::{expr::TypedSpanExpr, Type};
use llamac_utils::Spanned;

use crate::{Engine, InferResult};

impl Engine {
    /// Transform an expression into a typed expression, eagerly inferring the immediately obvious types, and generating a type variable and constraints for the rest
    pub(super) fn infer_expr(
        &mut self,
        expr: SpanExpr,
        ty: Spanned<Type>,
    ) -> InferResult<TypedSpanExpr> {
        todo!()
    }
}
