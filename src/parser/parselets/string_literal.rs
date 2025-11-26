use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses string literal expressions.
///
/// Example: `"hello world"`
pub struct StringLiteralParselet;

impl PrefixParselet for StringLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let lexeme = token
            .lexeme
            .ok_or_else(|| ParseError::Custom("String literal missing lexeme".to_string()))?;

        Ok(Expr::StringLiteral(lexeme))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

    crate::test_parselet!(
        StringLiteralParselet,
        test_parse_hello {
            input: "\"hello\"",
            want: Expr::StringLiteral(s),
            want_value: assert_eq!(s, "hello"),
        },
        test_parser_empty {
            input: "\"\"",
            want: Expr::StringLiteral(s),
            want_value: assert_eq!(s, ""),
        },
        test_parse_hello_world {
            input: "\"hello world\"",
            want: Expr::StringLiteral(s),
            want_value: assert_eq!(s, "hello world"),
        },
    );

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Str, None, Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = StringLiteralParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
