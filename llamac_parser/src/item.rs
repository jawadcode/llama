use llamac_ast::Item;

use llamac_utils::Spanned;

use crate::{error::SyntaxError, lexer::TK, ParseResult, Parser};

impl Iterator for Parser<'_> {
    type Item = ParseResult<Spanned<Item>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parse_item() {
            Ok(item) => Some(Ok(item)),
            Err(SyntaxError::End) => None,
            err @ Err(_) => Some(err),
        }
    }
}

impl Parser<'_> {
    fn parse_item(&mut self) -> ParseResult<Spanned<Item>> {
        Ok(match self.peek() {
            TK::Const => self.parse_const()?.map(Item::Const),
            TK::Fun => self.parse_fundef()?.map(Item::FunDef),
            TK::Eof => return Err(SyntaxError::End),
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "'fun' or 'const'".to_string(),
                    got: self.next_token()?,
                })
            }
        })
    }
}
