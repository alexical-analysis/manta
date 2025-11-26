use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses integer literal expressions.
///
/// Example: `42`
pub struct IntLiteralParselet;

impl PrefixParselet for IntLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let lexeme = token
            .lexeme
            .ok_or_else(|| ParseError::Custom("Integer literal missing lexeme".to_string()))?;

        let value = lexeme
            .parse::<i64>()
            .map_err(|_| ParseError::invalid_integer(&lexeme))?;

        Ok(Expr::IntLiteral(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

    crate::test_parselet!(
        IntLiteralParselet,
        test_parse_42 {
            input: "42",
            want: Expr::IntLiteral(42),
            want_value: (),
        },
        test_parse_0 {
            input: "0",
            want: Expr::IntLiteral(0),
            want_value: (),
        },
        test_parse_max {
            input: "9223372036854775807",
            want: Expr::IntLiteral(9223372036854775807),
            want_value: (),
        },
    );

    #[test]
    fn test_parse_invalid_integer() {
        let token = Token::new(
            TokenKind::Int,
            Some("99999999999999999999".to_string()),
            Span::new(0, 20),
        );
        let mut parser = Parser::new(Lexer::new(""));
        let result = IntLiteralParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Int, None, Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = IntLiteralParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
