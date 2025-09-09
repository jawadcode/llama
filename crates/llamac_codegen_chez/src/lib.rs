use std::fmt::{self, Display, Write};

use llamac_ast::expr::{BinOp, Literal, UnOp};
use llamac_typed_ast::{
    TypedItem, TypedSourceFile,
    expr::{
        InnerExpr, TypedBinaryOp, TypedClosure, TypedFunCall, TypedIfThenExpr, TypedList,
        TypedSpanExpr, TypedUnaryOp,
    },
    stmt::{TypedConst, TypedFunDef},
};
use llamac_utils::FmtItems;

const SCM_PRELUDE: &str = r#"
(import (rnrs))

;; I'm lazy
(define (noteq a b) (not (equal? a b)))
(define (fnpipe x f) (f x))
"#;

pub fn compile_file(f: &mut fmt::Formatter, tast: TypedSourceFile) -> fmt::Result {
    f.write_str(SCM_PRELUDE)?;

    for item in tast.items {
        f.write_str("(define ")?;
        match item.node {
            TypedItem::Const(TypedConst {
                name,
                annot: _,
                value,
            }) => {
                name.node.fmt(f)?;

                compile_expr(f, value)?;
                f.write_char(')')?;
            }
            TypedItem::FunDef(TypedFunDef {
                name,
                params,
                ret_ty: _,
                body,
            }) => {
                f.write_char('(')?;
                name.node.fmt(f)?;
                f.write_char(' ')?;
                FmtItems::new(
                    params.node.0.into_iter().map(|param| param.node.name.node),
                    " ",
                )
                .fmt(f)?;
                f.write_str(") ")?;
                compile_expr(f, body)?;
            }
        }
        f.write_char(')')?;
    }

    Ok(())
}

fn compile_expr(f: &mut fmt::Formatter, expr: TypedSpanExpr) -> fmt::Result {
    match (*expr.node).0 {
        InnerExpr::Ident(ident) => ident.fmt(f),
        InnerExpr::Literal(literal) => match literal {
            Literal::Unit => f.write_str("'unit"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Int(num) => num.fmt(f),
            Literal::Float(float) => float.fmt(f), // Let's assume the float format is same-ish
            Literal::Bool(b) => f.write_str(if b { "#t" } else { "#f" }),
        },
        InnerExpr::List(TypedList(list)) => {
            f.write_str("(list ")?;
            for item in list {
                compile_expr(f, item)?;
                f.write_char(' ')?;
            }
            f.write_char(')')
        }
        InnerExpr::UnaryOp(TypedUnaryOp { op, value }) => {
            f.write_char('(')?;
            f.write_str(match op.node {
                UnOp::Not => "not ",
                UnOp::INegate | UnOp::FNegate => "- ",
            })?;
            compile_expr(f, value)?;
            f.write_char(')')
        }
        InnerExpr::BinaryOp(TypedBinaryOp { op, lhs, rhs }) => {
            f.write_char('(')?;
            f.write_str(match op.node {
                BinOp::Add | BinOp::FAdd => "+ ",
                BinOp::Sub | BinOp::FSub => "- ",
                BinOp::Mul | BinOp::FMul => "* ",
                BinOp::Div | BinOp::FDiv => "/ ",
                BinOp::Mod => "mod ",
                BinOp::And => "and ",
                BinOp::Or => "or ",
                BinOp::Lt => "< ",
                BinOp::Leq => "<= ",
                BinOp::Gt => "> ",
                BinOp::Geq => ">= ",
                BinOp::Eq => "= ",
                BinOp::Neq => "noteq ",
                BinOp::Pipe => "fnpipe ",
                BinOp::Cons => "cons ",
            })?;
            f.write_char(' ')?;
            compile_expr(f, lhs)?;
            f.write_char(' ')?;
            compile_expr(f, rhs)?;
            f.write_char(')')
        }
        InnerExpr::FunCall(TypedFunCall { fun, args }) => {
            f.write_char('(')?;
            compile_expr(f, fun)?;
            f.write_char(' ')?;
            for arg in args.node.0 {
                compile_expr(f, arg)?;
                f.write_char(' ')?;
            }
            f.write_char(')')
        }
        InnerExpr::Closure(TypedClosure {
            params,
            ret_ty: _,
            body,
        }) => {
            f.write_str("(lambda (")?;
            FmtItems::new(params.node.0.into_iter().map(|param| param.node.name), ' ').fmt(f)?;
            f.write_str(") ")?;
            compile_expr(f, body)?;
            f.write_char(')')
        }
        InnerExpr::IfThen(TypedIfThenExpr { cond, then, r#else }) => {
            f.write_str("(if ")?;
            compile_expr(f, cond)?;
            f.write_char(' ')?;
            compile_expr(f, then)?;
            f.write_char(' ')?;
            compile_expr(f, r#else)?;
            f.write_char(')')
        }
        InnerExpr::Cond(_typed_cond_expr) => todo!(),
        InnerExpr::Match(_typed_match) => todo!(),
        InnerExpr::Block(_typed_block) => todo!(),
        InnerExpr::Stmt(_spanned) => todo!(),
    }
}
