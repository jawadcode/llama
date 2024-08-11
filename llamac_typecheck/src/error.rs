use std::{
    error::Error,
    fmt::{self, Display},
};

use llamac_typed_ast::Type;
use llamac_utils::{Ident, Span};

use ariadne::{sources, Label, Report, ReportKind};

#[derive(Debug, Clone)]
pub enum InferError {
    NotFound {
        ident: Ident,
        span: Span,
    },
    MissingElseBranch {
        span: Span,
    },
    TypeMismatch {
        got: Type,
        got_span: Span,
        expected: Type,
        expected_span: Span,
    },
    Ambiguous {
        ty: Type,
        span: Span,
    },
    InfiniteType {
        id: usize,
        ty: Type,
        id_span: Span,
        ty_span: Span,
    },
}

impl Display for InferError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl Error for InferError {}

impl InferError {
    pub fn report(self, filename: String, file: &str) {
        match self {
            InferError::NotFound { ident, span } => {
                Report::build(ReportKind::Error, &filename, span.start)
                    .with_message(format!("Cannot find `{ident}` in this scope"))
                    .with_label(
                        Label::new((filename.clone(), span.into()))
                            .with_message("not found in this scope"),
                    )
            }
            InferError::MissingElseBranch { span } => {
                Report::build(ReportKind::Error, &filename, span.start)
                    .with_message("Missing else branch")
                    .with_label(
                        Label::new((filename.clone(), span.into()))
                            .with_message("missing else branch"),
                    )
            }
            InferError::TypeMismatch {
                got,
                got_span,
                expected,
                expected_span,
            } => Report::build(ReportKind::Error, &filename, got_span.start)
                .with_message(format!("Type mismatch, expected {expected}, got {got}"))
                .with_label(
                    Label::new((filename.clone(), expected_span.into()))
                        .with_message(format!("expected {expected}")),
                )
                .with_label(
                    Label::new((filename.clone(), got_span.into()))
                        .with_message(format!("got {got}")),
                ),
            InferError::Ambiguous { ty, span } => {
                Report::build(ReportKind::Error, &filename, span.start)
                    .with_message(format!("Ambiguous type {ty}, try adding a type annotation"))
                    .with_label(
                        Label::new((filename.clone(), span.into()))
                            .with_message(format!("has unresolved type {ty}")),
                    )
            }
            InferError::InfiniteType {
                id,
                ty,
                id_span,
                ty_span,
            } => Report::build(ReportKind::Error, &filename, ty_span.start)
                .with_message(format!("Infinite type, T{id} occurs in {ty}"))
                .with_label(
                    Label::new((filename.clone(), id_span.into()))
                        .with_message(format!("type T{id} originates from here")),
                )
                .with_label(
                    Label::new((filename.clone(), ty_span.into()))
                        .with_message(format!("has the type {ty}")),
                ),
        }
        .finish()
        .eprint(sources(vec![(filename, file)]))
        .unwrap();
    }
}
