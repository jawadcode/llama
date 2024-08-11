use ariadne::{sources, Label, Report, ReportKind};
use llamac_utils::{Span, Spanned};

use crate::lexer::Token;

#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken { expected: String, got: Token },
    InvalidLiteral(Token),
    InvalidEscSeq(Spanned<char>),
    UnexpectedEof(Span),
    UnknownType(Token),
    // Not an actual error 🤫
    End,
}

impl SyntaxError {
    pub fn report(&self, filename: String, file: &str) {
        match self {
            SyntaxError::UnexpectedToken { expected, got } => {
                Report::build(ReportKind::Error, &filename, got.span.start)
                    .with_message("Unexpected token")
                    .with_label(
                        Label::new((filename.clone(), got.span.into()))
                            .with_message(format!("Expected {expected}, got {got}")),
                    )
            }
            SyntaxError::InvalidLiteral(t) => {
                Report::build(ReportKind::Error, &filename, t.span.start)
                    .with_message("Invalid literal")
                    .with_label(
                        Label::new((filename.clone(), t.span.into()))
                            .with_message(format!("Invalid literal '{}'", t.text(file))),
                    )
            }
            SyntaxError::InvalidEscSeq(c) => {
                Report::build(ReportKind::Error, &filename, c.span.start)
                    .with_message("Invalid escape sequence")
                    .with_label(
                        Label::new((filename.clone(), c.span.into()))
                            .with_message(format!("Invalid escape sequence '\\{}'", c.node)),
                    )
            }
            SyntaxError::UnexpectedEof(span) => {
                Report::build(ReportKind::Error, &filename, span.start)
                    .with_message("Unexpected EOF")
                    .with_label(
                        Label::new((filename.clone(), span.start..span.end))
                            .with_message("Unexpected EOF"),
                    )
            }
            SyntaxError::UnknownType(token) => {
                Report::build(ReportKind::Error, &filename, token.span.start)
                    .with_message("Unknown type")
                    .with_label(
                        Label::new((filename.clone(), token.span.into()))
                            .with_message(format!("Unknown type {}", token.text(file))),
                    )
            }
            SyntaxError::End => {
                unreachable!("You shouldn't be seeing this :(")
            }
        }
        .finish()
        .eprint(sources(vec![(filename, file)]))
        .unwrap()
    }
}
