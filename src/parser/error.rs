use ariadne::{sources, Label, Report, ReportKind};

use crate::{ast::Spanned, lexer::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    UnexpectedToken {
        expected: String,
        got: Token,
    },
    InvalidLiteral(Token),
    InvalidEscSeq(Spanned<char>),
    UnexpectedEof(Token),
    /// Not an actual error
    End,
}

pub type ParseResult<T> = Result<T, SyntaxError>;

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
            SyntaxError::UnexpectedEof(t) => {
                Report::build(ReportKind::Error, &filename, t.span.start)
                    .with_message("Unexpected EOF")
                    .with_label(
                        Label::new((filename.clone(), t.span.into()))
                            .with_message("Unexpected EOF"),
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

#[cfg(test)]
mod tests {
    /*
        use crate::lexer::{Token, TK};
        use super::SyntaxError;

        fn unexpected_token() {
            let filename = "testing.lm".to_string();
            let file = r"fun main() hello do
        do_thing()
    end";
            let span = 11..16;
            let token = Token {
                kind: TK::Ident,
                span: span.into(),
            };

            SyntaxError::UnexpectedToken {
                expected: "do".to_string(),
                got: token,
            }
            .report(filename, file)
        }
      */
}
