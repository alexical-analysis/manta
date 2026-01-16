use crate::ast::{Expr, MetaTypeExpr};
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixExprParselet;
use crate::parser::types;
use crate::parser::{ParseError, Parser};

/// Parses unary negation expressions.
///
/// Example: `@i32`
/// Example: `@[50]Vec3`
/// Example: `@[]str`
/// Example: `@Person`
pub struct MetaTypeParselet;

impl PrefixExprParselet for MetaTypeParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        let token = parser.consume()?;
        let type_spec = types::parse_type(parser, token)?;

        Ok(Expr::MetaType(MetaTypeExpr { type_spec }))
    }
}
