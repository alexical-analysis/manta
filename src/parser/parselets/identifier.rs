use crate::ast::{Expr, IdentifierExpr};
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses identifier expressions.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierParselet;

impl PrefixParselet for IdentifierParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let name = token
            .lexeme
            .ok_or_else(|| ParseError::Custom("Identifier missing lexeme".to_string()))?;

        Ok(Expr::Identifier(IdentifierExpr { name }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

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

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Ident, None, Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = IdentifierParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
