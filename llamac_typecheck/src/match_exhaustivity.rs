use llamac_typed_ast::{
    expr::{InnerExpr, TypedBinaryOp, TypedList, TypedMatch, TypedSpanExpr, TypedUnaryOp},
    stmt::{TypedConst, TypedFunDef},
    TypedItem, TypedSourceFile,
};
use llamac_utils::{concat, Span};

pub fn check_exhaustivity(TypedSourceFile { path: _, items }: &TypedSourceFile) -> Vec<Span> {
    for item in items {
        let errors = match &item.node {
            TypedItem::Const(r#const) => check_const(r#const),
            TypedItem::FunDef(fun_def) => check_fun_def(fun_def),
        };
        if !errors.is_empty() {
            return errors;
        }
    }

    Vec::new()
}

fn check_const(
    TypedConst {
        name: _,
        annot: _,
        value,
    }: &TypedConst,
) -> Vec<Span> {
    check_expr(value)
}

fn check_fun_def(
    TypedFunDef {
        name: _,
        params: _,
        ret_ty: _,
        body,
    }: &TypedFunDef,
) -> Vec<Span> {
    check_expr(body)
}

fn check_expr(value: &TypedSpanExpr) -> Vec<Span> {
    match &value.node.0 {
        InnerExpr::List(TypedList(list)) => concat(list.iter().map(check_expr)),
        InnerExpr::UnaryOp(TypedUnaryOp { op: _, value }) => check_expr(value),
        InnerExpr::BinaryOp(TypedBinaryOp { op: _, lhs, rhs }) => {
            concat([lhs, rhs].map(check_expr))
        }
        InnerExpr::FunCall(_) => todo!(),
        InnerExpr::Closure(_) => todo!(),
        InnerExpr::IfThen(_) => todo!(),
        InnerExpr::Cond(_) => todo!(),
        InnerExpr::Match(_) => todo!(),
        InnerExpr::Block(_) => todo!(),
        InnerExpr::Stmt(_) => todo!(),
        _ => Vec::new(),
    }
}

fn check_match(_match: TypedMatch) -> Vec<Span> {
    Vec::new()
}
