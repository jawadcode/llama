use derive_more::Display;
use std::{collections::HashMap, env, os, path::PathBuf};

use crate::{
    ast::{Expr, Lit, SourceFile, SpanExpr, Stmt, TyIdent},
    lexer::{Span, TK},
    parser::error::SyntaxError,
    utils::join_sep,
};

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Type {
    #[display(fmt = "Number")]
    Num,
    #[display(fmt = "String")]
    Str,
    #[display(fmt = "Bool")]
    Bool,
    #[display(fmt = "Unit")]
    Unit,
    #[display(fmt = "Fun[{}] -> {}", r#"join_sep(_0, ", ")"#, _1)]
    Fun(Vec<Self>, Box<Self>),
}

impl From<&TyIdent> for Type {
    fn from(ty_ident: &TyIdent) -> Self {
        match ty_ident {
            TyIdent::Num => Type::Num,
            TyIdent::Str => Type::Str,
            TyIdent::Bool => Type::Bool,
            TyIdent::Unit => Type::Unit,
            TyIdent::Fun(params, ret_type) => Type::Fun(
                params.iter().map(Into::into).collect::<Vec<_>>(),
                Box::new(ret_type.as_ref().into()),
            ),
        }
    }
}

/// A type error
#[derive(Debug)]
pub enum TypeError {
    ExpectedGot {
        span: Span,
        expected: Type,
        got: Type,
    },
    ExpectedFunction {
        span: Span,
        got: Type,
    },
    AnnotationRequired {
        span: Span,
    },
}

pub type TypeResult<T> = Result<T, TypeError>;

/// Checks the type of a given expression
pub struct TypeChecker {
    /// Source file
    source: SourceFile,
    /// A stack of the scopes
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeChecker {
    pub fn new(source: SourceFile) -> Self {
        Self {
            source,
            scopes: vec![HashMap::new()],
        }
    }

    pub fn check(&mut self) -> TypeResult<()> {
        for def in &self.source.defs {
            match &def.node {
                Stmt::FunDef {
                    ident,
                    params,
                    ret_type,
                    body: _,
                } => {
                    let value = Type::Fun(
                        params.iter().map(|(_, ty)| ty.into()).collect::<Vec<_>>(),
                        Box::new(ret_type.into()),
                    );
                    self.scopes[0].insert(ident.to_string(), value);
                }
                Stmt::Let {
                    ident,
                    annot,
                    expr: _,
                } => {
                    if let Some(annot) = annot {
                        self.scopes[0].insert(ident.to_string(), annot.into());
                    } else {
                        return Err(TypeError::AnnotationRequired { span: def.span });
                    }
                }
            }
        }
        dbg!(&self.scopes);
        Ok(())
    }
}
/*impl TypeChecker {
    /// Infer the type of `self.expr` and check that it is `ty`
    pub fn check(&mut self, expr: &SpanExpr, ty: Type) -> Result<(), String> {
        let type_result = self.infer(expr)?;
        if type_result == ty {
            Ok(())
        } else {
            Err(format!("Expected {ty}, got {type_result}"))
        }
    }

    /// Infer the type of `self.expr` and check that it is a function
    fn check_fun(&mut self, expr: &SpanExpr) -> Result<(Vec<Type>, Type), String> {
        let type_result = self.infer(expr)?;
        if let Type::Fun(args, ret) = type_result {
            Ok((args, *ret))
        } else {
            Err(format!("Expected function, got {type_result}"))
        }
    }

    /// Infer the type of `self.expr`
    pub fn infer(&mut self, expr: &SpanExpr) -> Result<Type, String> {
        match &expr.node {
            Expr::Lit(lit) => Ok(match lit {
                Lit::Str(_) => Type::Str,
                Lit::Int(_) => Type::Num,
                Lit::Float(_) => Type::Num,
                Lit::Bool(_) => Type::Bool,
                Lit::Unit => Type::Unit,
            }),
            Expr::Ident(ident) => self
                .gamma
                .get(ident)
                .map(Clone::clone)
                .ok_or(format!("{ident} is undefined")),
            Expr::UnOp { op, operand } => match op {
                TK::Sub => self.check(operand, Type::Num).map(|_| Type::Num),
                TK::Not => self.check(operand, Type::Bool).map(|_| Type::Bool),
                _ => unreachable!(),
            },
            Expr::BinOp { op, lhs, rhs } => match op {
                TK::Less | TK::LessEq | TK::Greater | TK::GreatEq => match self.infer(lhs)? {
                    Type::Num => self.check(rhs, Type::Num).and(Ok(Type::Bool)),
                    Type::Str => self.check(rhs, Type::Str).and(Ok(Type::Bool)),
                    ty => Err(format!("Expected Number or String, got {ty}")),
                },
                TK::Add => match self.infer(lhs)? {
                    Type::Num => self.check(rhs, Type::Num).and(Ok(Type::Num)),
                    Type::Str => self.check(rhs, Type::Str).and(Ok(Type::Str)),
                    ty => Err(format!("Expected Number or string, got {ty}")),
                },
                TK::Sub | TK::Mul | TK::Div => {
                    self.check(lhs, Type::Num)?;
                    self.check(rhs, Type::Num)?;
                    Ok(Type::Num)
                }
                TK::And | TK::Or => {
                    self.check(lhs, Type::Bool)?;
                    self.check(rhs, Type::Bool)?;
                    Ok(Type::Bool)
                }
                TK::Eq | TK::NotEq => {
                    let lhs_type = self.infer(lhs)?;
                    self.check(rhs, lhs_type)?;
                    Ok(Type::Bool)
                }
                _ => unreachable!(),
            },
            Expr::If { cond, then, elss } => {
                self.check(cond, Type::Bool)?;
                let then_type = self.infer(then)?;
                let else_type = self.infer(elss)?;

                if then_type == else_type {
                    Ok(then_type)
                } else {
                    Err(format!(
                        "Mismatch between if clause ({then_type}) and else clause ({else_type})"
                    ))
                }
            }
            Expr::Match { expr, arms } => {
                todo!("Would have to make changes to parser to get this working properly")
            }
            Expr::Call { fun, args } => {
                let (args_types, return_type) = self.check_fun(fun)?;
                if args.len() != args_types.len() {
                    return Err(format!(
                        "Arity mismatch, expected {} args, got {} args",
                        args_types.len(),
                        args.len()
                    ));
                }

                for (arg, arg_type) in args.iter().zip(args_types) {
                    self.check(arg, arg_type)?;
                }

                Ok(return_type)
            }
            Expr::Block { exprs } => exprs
                .last()
                .map(|last| self.infer(last))
                .unwrap_or(Ok(Type::Unit)),
            Expr::Closure { params, body } => {
                todo!("Would have to infer type from usage, idk how to do that currently")
            }
            Expr::Stmt(_) => Ok(Type::Unit),
        }
    }
}*/
/*
#[cfg(test)]
mod tests {
    use std::io::{self, Write};

    use crate::parser::Parser;

    use super::TypeChecker;

//    #[test]
    fn test() {
        loop {
            let mut input = String::new();
            print!("> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            let expr = Parser::new(&input).expr().unwrap();
            let mut checker = TypeChecker::default();
            match checker.infer(&expr) {
                Ok(ty) => println!("Type: {ty}"),
                Err(err) => eprintln!("Could not infer type: {err}"),
            }
        }
    }
}*/
