use crate::ast::{Expr, MetaTypeExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token};
use crate::parser::types;

/// Parses unary negation expressions.
///
/// Example: `@i32`
/// Example: `@[50]Vec3`
/// Example: `@[]str`
/// Example: `@Person`
pub struct MetaTypeParselet;

impl PrefixExprParselet for MetaTypeParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        let token = lexer.next_token();
        let type_spec = types::parse_type(lexer, token)?;

        Ok(Expr::MetaType(MetaTypeExpr { type_spec }))
    }
}
