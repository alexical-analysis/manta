use crate::ast::Expr;
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses grouped expressions enclosed in parentheses.
///
/// Example: `(a + b)`
pub struct GroupParselet;

impl PrefixParselet for GroupParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        // Parse the inner expression
        let expr = match parser.parse_expression() {
            Ok(expr) => expr,
            Err(_) => {
                return Err(ParseError::MissingExpression(
                    "missing expression inside parentheses".to_string(),
                ));
            }
        };

        // Expect a closing ')'
        let matched = parser.match_token(TokenKind::CloseParen)?;
        if !matched {
            return Err(ParseError::UnexpectedToken("expected ')'".to_string()));
        }

        // Make sure we consume the ')' or we'll get an error
        parser.consume().unwrap();

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::Lexer;

    crate::test_parselet!(
        GroupParselet,
        test_parse_grouped_int {
            input: "(42)",
            want: Expr::IntLiteral(42),
            want_value: (),
        },
        test_parse_grouped_bool {
            input: "(true)",
            want: Expr::BoolLiteral(true),
            want_value: (),
        },
    );

    #[test]
    fn test_missing_closing_paren() {
        let mut lexer = Lexer::new("(42");
        let token = lexer.next_token().unwrap();

        let mut parser = Parser::new(lexer);
        let result = GroupParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_group() {
        let mut lexer = Lexer::new("()");
        let token = lexer.next_token().unwrap();

        let mut parser = Parser::new(lexer);
        let result = GroupParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
