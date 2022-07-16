use derive_more::Display;
use std::collections::HashMap;

use crate::{
    ast::{SourceFile, Stmt, TyIdent},
    lexer::Span,
    utils::join_sep,
};

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Type {
    #[display(fmt = "Number")]
    Number,
    #[display(fmt = "String")]
    String,
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
            TyIdent::Number => Type::Number,
            TyIdent::String => Type::String,
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
