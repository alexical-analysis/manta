use crate::ast::Expr;
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parses literal expressions.
///
/// Example: `42`
/// Example: `true`
/// Example: `"hello world!"`
pub struct LiteralParselet;

impl PrefixExprParselet for LiteralParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Expr, ParseError> {
        match token.kind {
            TokenKind::Int => parse_int(lexer, token),
            TokenKind::Float => parse_float(lexer, token),
            TokenKind::TrueLiteral => Ok(Expr::BoolLiteral(true)),
            TokenKind::FalseLiteral => Ok(Expr::BoolLiteral(false)),
            TokenKind::Str => Ok(Expr::StringLiteral(token.lexeme_id)),
            e => Err(ParseError::Custom(
                token,
                format!("invalid integer {:?}", e),
            )),
        }
    }
}

fn parse_int(lexer: &mut Lexer, token: Token) -> Result<Expr, ParseError> {
    let integer_str = lexer.lexeme(token.lexeme_id);
    let integer_str = integer_str.replace("_", "");

    match integer_str.parse() {
        Ok(n) => Ok(Expr::IntLiteral(n)),
        Err(e) => Err(ParseError::Custom(
            token,
            format!("invalid integer {:?}", e),
        )),
    }
}

fn parse_float(lexer: &mut Lexer, token: Token) -> Result<Expr, ParseError> {
    match lexer.lexeme(token.lexeme_id).parse() {
        Ok(f) => Ok(Expr::FloatLiteral(f)),
        Err(e) => Err(ParseError::Custom(token, format!("invalid float {:?}", e))),
    }
}
