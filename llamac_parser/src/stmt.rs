use llamac_ast::{
    expr::Expr,
    stmt::{Const, FunDef, FunParam, FunParams, LetBind, SpanStmt, Stmt, Type, Types},
};

use llamac_utils::{spanned, Span, Spanned};

use crate::{error::SyntaxError, lexer::TK, ParseResult, Parser};

impl Parser<'_> {
    pub(super) fn parse_stmt(&mut self) -> ParseResult<SpanStmt> {
        Ok(match self.peek() {
            TK::Const => self.parse_const()?.map(Stmt::Const),
            TK::Let => self.parse_let()?.map(Stmt::LetBind),
            TK::Fun => self.parse_fundef()?.map(Stmt::FunDef),
            TK::If => self.parse_if()?.map(Stmt::IfThen),
            TK::Cond => self.parse_cond()?.map(Stmt::Cond),
            TK::Match => self.parse_match()?.map(Stmt::Match),
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: "statement".to_string(),
                    got: self.next_token()?,
                })
            }
        })
    }

    pub(super) fn parse_const(&mut self) -> ParseResult<Spanned<Const>> {
        let r#const = self.expect(TK::Const)?;
        let name = self.parse_ident()?;
        self.expect(TK::Colon)?;
        let annot = self.parse_type()?;
        self.expect(TK::Assign)?;
        let value = self.parse_expr()?;
        Ok(spanned! {r#const.span + value.span, Const { name, annot, value }})
    }

    fn parse_let(&mut self) -> ParseResult<Spanned<LetBind>> {
        let r#let = self.expect(TK::Let)?;
        let name = self.parse_ident()?;
        let annot = if self.at(TK::Colon) {
            self.lexer.next().unwrap();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TK::Assign)?;
        let value = self.parse_expr()?;
        Ok(spanned! {r#let.span + value.span, LetBind { name, annot, value }})
    }

    pub(super) fn parse_fundef(&mut self) -> ParseResult<Spanned<FunDef>> {
        let fun = self.expect(TK::Fun)?;
        let name = self.parse_ident()?;
        let lparen = self.expect(TK::LParen)?;
        let mut params = Vec::new();
        while !self.at(TK::RParen) {
            let name = self.parse_ident()?;
            self.expect(TK::Colon)?;
            let annot = self.parse_type()?;
            params.push(spanned! {name.span + annot.span, FunParam { name, annot }});
        }
        let rparen = self.expect(TK::RParen)?;
        let params = spanned! {lparen.span + rparen.span, FunParams(params)};
        let ret_ty = if self.at(TK::Colon) {
            self.lexer.next().unwrap();
            self.parse_type()?
        } else {
            spanned! {rparen.span.end..rparen.span.end, Type::Unit}
        };
        match self.peek() {
            TK::Assign => {
                self.lexer.next().unwrap();
                let body = self.parse_expr()?;
                Ok(spanned! {fun.span.start..body.span.end, FunDef { name, params, ret_ty, body }})
            }
            TK::Do => {
                let body = self
                    .parse_block()?
                    .map(|block| Box::new(Expr::Block(block)));
                Ok(spanned! {fun.span.start..body.span.end, FunDef { name, params, ret_ty, body }})
            }
            _ => Err(SyntaxError::UnexpectedToken {
                expected: "'=' or block expression".to_string(),
                got: self.next_token()?,
            }),
        }
    }

    pub(super) fn parse_type(&mut self) -> ParseResult<Spanned<Type>> {
        let name = self.expect(TK::Ident)?;
        let text = name.text(self.source);
        match text {
            "Unit" => Ok(spanned! {name.span, Type::Unit}),
            "Bool" => Ok(spanned! {name.span, Type::Bool}),
            "String" => Ok(spanned! {name.span, Type::String}),
            "Int" => Ok(spanned! {name.span, Type::Int}),
            "Float" => Ok(spanned! {name.span, Type::Float}),
            "Fun" => self.parse_type_fun(name.span),
            "List" => self.parse_type_list(name.span),
            _ => Err(SyntaxError::UnexpectedToken {
                expected: "type".to_string(),
                got: name,
            }),
        }
    }

    fn parse_type_fun(&mut self, name: Span) -> ParseResult<Spanned<Type>> {
        let lsquare = self.expect(TK::LSquare)?;
        let mut params = Vec::new();
        while !self.at(TK::RSquare) {
            let param = self.parse_type()?;
            params.push(param);
        }
        let rsquare = self.expect(TK::RSquare)?;
        let params = spanned! {lsquare.span + rsquare.span, Types(params)};
        self.expect(TK::Arrow)?;
        let ret_ty = self.parse_type()?.map(Box::new);
        Ok(spanned! {name + rsquare.span, Type::Fun { params, ret_ty }})
    }

    fn parse_type_list(&mut self, name: Span) -> ParseResult<Spanned<Type>> {
        self.expect(TK::LSquare)?;
        let item = self.parse_type()?.map(Box::new);
        let rsquare = self.expect(TK::RSquare)?;
        Ok(spanned! {name + rsquare.span, Type::List(item)})
    }
}
