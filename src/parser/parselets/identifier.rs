use crate::ast::{Expr, IdentifierExpr};
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses identifier expressions.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierParselet;

impl PrefixExprParselet for IdentifierParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        Ok(Expr::Identifier(IdentifierExpr { name: token.lexeme }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::test_parselet!(
        IdentifierParselet,
        test_parse_simple_identifier {
            input: "foo",
            want: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "foo"),
        },
        test_parse_variable_name {
            input: "my_variable",
            want: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "my_variable"),
        },
        test_parse_identifier_with_numbers {
            input: "var123",
            want: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "var123"),
        },
    );
}
