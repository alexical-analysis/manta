use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses floating-point literal expressions.
///
/// Example: `3.14`
pub struct FloatLiteralParselet;

impl PrefixExprParselet for FloatLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let value = token
            .lexeme
            .parse::<f64>()
            .map_err(|_| ParseError::invalid_float(&token.lexeme))?;

        Ok(Expr::FloatLiteral(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

    crate::test_parselet!(
        FloatLiteralParselet,
        test_parse_3_14 {
            input: "3.45",
            want: Expr::FloatLiteral(3.45),
            want_value: (),
        },
        test_parse_0_0 {
            input: "0.0",
            want: Expr::FloatLiteral(0.0),
            want_value: (),
        },
        test_parse_1_23e4 {
            input: "1.23e4",
            want: Expr::FloatLiteral(12300.0),
            want_value: (),
        },
    );

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Float, String::new(), Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = FloatLiteralParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
