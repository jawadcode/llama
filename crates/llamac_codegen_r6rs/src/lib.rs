use std::io::Write;

use llamac_ast::expr::{BinOp, Literal, MatchPattern, UnOp};
use llamac_typed_ast::{
    TypedItem, TypedSourceFile,
    expr::{
        InnerExpr, TypedBinaryOp, TypedBlock, TypedClosure, TypedCondArm, TypedCondArms,
        TypedCondExpr, TypedFunCall, TypedIfThenExpr, TypedList, TypedMatch, TypedMatchArm,
        TypedMatchArms, TypedSpanExpr, TypedUnaryOp,
    },
    stmt::{TypedCondStmt, TypedConst, TypedFunDef, TypedIfThenStmt, TypedLetBind, TypedStmt},
};
use llamac_utils::{FmtItems, Spanned};

const SCM_PRELUDE: &[u8] = br#"#!r6rs

#| === BEGIN PRELUDE === |#
(import (rnrs))

(define (ref x) x)
(define (deref x) x) ; ref and deref are purely for debuggability
(define (noteq a b) (not (equal? a b)))
(define (fnpipe x f) (f x))
(define (snoc l a) `(,l . ,a))
(define (concat a b) (append b a))

(define print display)
(define print_int display)
(define print_float display)
(define (print_list_ints l) (display (reverse l)))
#| === END PRELUDE === |#

"#;

pub fn compile_file<W: Write>(writer: &mut W, tast: TypedSourceFile) -> std::io::Result<()> {
    writer.write_all(SCM_PRELUDE)?;

    for item in tast.items {
        writer.write_all(b"(define ")?;
        match item.node {
            TypedItem::Const(TypedConst {
                name,
                annot: _,
                value,
            }) => {
                write!(writer, "{name}")?;
                writer.write_all(b" ")?;
                compile_expr(writer, &value.node.0)?;
            }
            TypedItem::FunDef(TypedFunDef {
                name,
                params,
                ret_ty: _,
                body,
            }) => {
                writer.write_all(b"(")?;
                write!(writer, "{name}")?;
                if !params.node.0.is_empty() {
                    writer.write_all(b" ")?;
                }
                write!(
                    writer,
                    "{}",
                    FmtItems::new(
                        params.node.0.into_iter().map(|param| param.node.name.node),
                        " ",
                    )
                )?;
                writer.write_all(b") ")?;
                compile_expr(writer, &body.node.0)?;
            }
        }
        writer.write_all(b")\n")?;
    }

    writer.write_all(b"(main)\n")
}

fn compile_expr<W: Write>(writer: &mut W, expr: &InnerExpr) -> std::io::Result<()> {
    match expr {
        InnerExpr::Ident(ident) => write!(writer, "{ident}"),
        InnerExpr::Literal(literal) => compile_literal(writer, literal),
        InnerExpr::List(TypedList(list)) => {
            writer.write_all(b"(list")?;
            for item in list {
                writer.write_all(b" ")?;
                compile_expr(writer, &item.node.0)?;
            }
            writer.write_all(b")")
        }
        InnerExpr::UnaryOp(TypedUnaryOp { op, value }) => {
            writer.write_all(b"(")?;
            writer.write_all(match op.node {
                UnOp::Ref => b"ref ",
                UnOp::Deref => b"deref ",
                UnOp::Not => b"not ",
                UnOp::INegate | UnOp::FNegate => b"- ",
            })?;
            compile_expr(writer, &value.node.0)?;
            writer.write_all(b")")
        }
        InnerExpr::BinaryOp(TypedBinaryOp { op, lhs, rhs }) => {
            writer.write_all(b"(")?;
            writer.write_all(match op.node {
                BinOp::Add | BinOp::FAdd => b"+",
                BinOp::Sub | BinOp::FSub => b"-",
                BinOp::Mul | BinOp::FMul => b"*",
                BinOp::Div | BinOp::FDiv => b"/",
                BinOp::Mod => b"mod",
                BinOp::And => b"and",
                BinOp::Or => b"or",
                BinOp::Lt => b"<",
                BinOp::Leq => b"<=",
                BinOp::Gt => b">",
                BinOp::Geq => b">=",
                BinOp::Eq => b"=",
                BinOp::Neq => b"noteq",
                BinOp::Pipe => b"fnpipe",
                BinOp::Append => b"snoc",
                BinOp::Concat => b"concat",
                BinOp::Assign => b"set!",
            })?;
            writer.write_all(b" ")?;
            compile_expr(writer, &lhs.node.0)?;
            writer.write_all(b" ")?;
            compile_expr(writer, &rhs.node.0)?;
            writer.write_all(b")")
        }
        InnerExpr::FunCall(TypedFunCall { fun, args }) => {
            writer.write_all(b"(")?;
            compile_expr(writer, &fun.node.0)?;
            writer.write_all(b" ")?;
            match &args.node.0[..] {
                [] => (),
                [sole] => compile_expr(writer, &sole.node.0)?,
                [first, rest @ ..] => {
                    compile_expr(writer, &first.node.0)?;
                    for arg in rest {
                        writer.write_all(b" ")?;
                        compile_expr(writer, &arg.node.0)?;
                    }
                }
            }
            writer.write_all(b")")
        }
        InnerExpr::Closure(TypedClosure {
            params,
            ret_ty: _,
            body,
        }) => {
            writer.write_all(b"(lambda (")?;
            write!(
                writer,
                "{}",
                FmtItems::new(params.node.0.iter().map(|param| &param.node.name), ' ')
            )?;
            writer.write_all(b") ")?;
            compile_expr(writer, &body.node.0)?;
            writer.write_all(b")")
        }
        InnerExpr::IfThen(TypedIfThenExpr { cond, then, r#else }) => {
            compile_if_then(writer, cond, then, Some(r#else))
        }
        InnerExpr::Cond(TypedCondExpr { arms, r#else }) => compile_cond(writer, arms, Some(r#else)),
        InnerExpr::Match(TypedMatch { examinee, arms }) => compile_match(writer, examinee, arms),
        InnerExpr::Block(TypedBlock { exprs, tail }) => {
            writer.write_all(b"(begin")?;
            let mut num_bindings = 0;
            let mut exprs = exprs.iter();
            if let Some(first) = exprs.next() {
                writer.write_all(b" ")?;
                compile_block_expr(writer, &mut num_bindings, first)?;
            }
            for expr in exprs {
                writer.write_all(b" ")?;
                compile_block_expr(writer, &mut num_bindings, expr)?;
            }
            if let Some(tail) = tail {
                writer.write_all(b" ")?;
                compile_expr(writer, &tail.node.0)?;
            } else {
                writer.write_all(b" 'unit")?;
            }
            writer.write_all(&b")".repeat(num_bindings + 1))
        }
        InnerExpr::Stmt(stmt) => compile_stmt(writer, &stmt.node),
    }
}

fn compile_block_expr<W: Write>(
    writer: &mut W,
    num_bindings: &mut usize,
    expr: &TypedSpanExpr,
) -> std::io::Result<()> {
    match &expr.node.0 {
        InnerExpr::Stmt(Spanned {
            span: _,
            node:
                TypedStmt::LetBind(TypedLetBind {
                    name,
                    annot: _,
                    value,
                })
                | TypedStmt::Const(TypedConst {
                    name,
                    annot: _,
                    value,
                }),
        }) => {
            *num_bindings += 1;
            writer.write_all(b"(let ((")?;
            write!(writer, "{name}")?;
            writer.write_all(b" ")?;
            compile_expr(writer, &value.node.0)?;
            writer.write_all(b"))")
        }
        InnerExpr::Stmt(Spanned {
            span: _,
            node:
                TypedStmt::FunDef(TypedFunDef {
                    name,
                    params,
                    ret_ty: _,
                    body,
                }),
        }) => {
            *num_bindings += 1;
            writer.write_all(b"(let ((")?;
            write!(writer, "{name}")?;
            writer.write_all(b" (lambda (")?;
            write!(
                writer,
                "{}",
                FmtItems::new(params.node.0.iter().map(|param| &param.node.name.node), " ",)
            )?;
            writer.write_all(b") ")?;
            compile_expr(writer, &body.node.0)?;
            writer.write_all(b")))")
        }
        _ => compile_expr(writer, &expr.node.0),
    }
}

fn compile_stmt<W: Write>(writer: &mut W, stmt: &TypedStmt) -> std::io::Result<()> {
    match stmt {
        TypedStmt::Const(_) | TypedStmt::FunDef(_) | TypedStmt::LetBind(_) => unreachable!(),
        TypedStmt::IfThen(TypedIfThenStmt { cond, then, r#else }) => {
            compile_if_then(writer, cond, then, r#else.as_ref())
        }
        TypedStmt::Cond(TypedCondStmt { arms, r#else }) => {
            compile_cond(writer, arms, r#else.as_ref())
        }
        TypedStmt::Match(TypedMatch { examinee, arms }) => compile_match(writer, examinee, arms),
    }
}

fn compile_if_then<W: Write>(
    writer: &mut W,
    cond: &TypedSpanExpr,
    then: &TypedSpanExpr,
    r#else: Option<&TypedSpanExpr>,
) -> std::io::Result<()> {
    writer.write_all(b"(if ")?;
    compile_expr(writer, &cond.node.0)?;
    writer.write_all(b" ")?;
    compile_expr(writer, &then.node.0)?;
    if let Some(r#else) = r#else {
        writer.write_all(b" ")?;
        compile_expr(writer, &r#else.node.0)?;
    }
    writer.write_all(b")")
}

fn compile_cond<W: Write>(
    writer: &mut W,
    Spanned {
        span: _,
        node: TypedCondArms(arms),
    }: &Spanned<TypedCondArms>,
    r#else: Option<&TypedSpanExpr>,
) -> std::io::Result<()> {
    writer.write_all(b"(cond ")?;
    for TypedCondArm { cond, target } in arms.iter().map(|arm| &arm.node) {
        writer.write_all(b"[")?;
        compile_expr(writer, &cond.node.0)?;
        writer.write_all(b" ")?;
        compile_expr(writer, &target.node.0)?;
        writer.write_all(b"]")?;
    }
    if let Some(r#else) = r#else {
        writer.write_all(b"[else ")?;
        compile_expr(writer, &r#else.node.0)?;
        writer.write_all(b"]")?;
    }
    writer.write_all(b")")
}

fn compile_match<W: Write>(
    writer: &mut W,
    examinee: &TypedSpanExpr,
    Spanned {
        span: _,
        node: TypedMatchArms(arms),
    }: &Spanned<TypedMatchArms>,
) -> std::io::Result<()> {
    // TODO: Instead of using `(case ...)`, implement using `cond`.
    // Will require more work but `(case ...)` seems to be quite restricted.
    // Ideally we want to have stuff like ranges and flexible list patterns.
    writer.write_all(b"(case ")?;
    compile_expr(writer, &examinee.node.0)?;
    writer.write_all(b" ")?;
    let mut arms = arms.iter();
    if let Some(first) = arms.next() {
        compile_match_arm(writer, first)?;
    }
    for arm in arms {
        writer.write_all(b" ")?;
        compile_match_arm(writer, arm)?;
    }
    writer.write_all(b")")
}

fn compile_match_arm<W: Write>(
    writer: &mut W,
    Spanned {
        span: _,
        node: TypedMatchArm { patterns, target },
    }: &Spanned<TypedMatchArm>,
) -> std::io::Result<()> {
    writer.write_all(b"(")?;
    match &patterns.node.0[..] {
        [] => unreachable!(),
        [sole] => compile_pattern(writer, sole)?,
        [first, rest @ ..] => {
            writer.write_all(b"(")?;
            compile_pattern(writer, first)?;
            for pattern in rest {
                writer.write_all(b" ")?;
                compile_pattern(writer, pattern)?;
            }
            writer.write_all(b")")?
        }
    }
    writer.write_all(b" ")?;
    compile_expr(writer, &target.node.0)?;
    writer.write_all(b")")
}

fn compile_pattern<W: Write>(
    writer: &mut W,
    pattern: &Spanned<MatchPattern>,
) -> std::io::Result<()> {
    match &pattern.node {
        MatchPattern::Wildcard => todo!("do proper match compilation"),
        MatchPattern::NamedWildcard(_ident) => todo!("do proper match compilation"),
        MatchPattern::Literal(literal) => compile_literal(writer, literal),
    }
}

fn compile_literal<W: Write>(writer: &mut W, literal: &Literal) -> std::io::Result<()> {
    match literal {
        Literal::Unit => writer.write_all(b"'unit"),
        Literal::String(s) => write!(writer, "\"{s}\""),
        Literal::Int(num) => write!(writer, "{num}"),
        Literal::Float(float) => write!(writer, "{float}"), // Let's assume the float format is same-ish
        Literal::Bool(b) => writer.write_all(if *b { b"#t" } else { b"#f" }),
    }
}
