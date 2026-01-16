use crate::ast::Pattern;
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser};

// parse manta patterns
//
// Example `mod::Enum.Variant`
// Example `mod::Enum.Variant(ident)`
// Example `mod::Type`
// Example `mod::Type(ident)`
//
// Example `Enum.Variant`
// Example `Enum.Variant(ident)`
//
// Example `.Variant`
// Example `.Variant(ident)`
//
// Example `Type`
// Example `Type(ident)`
//
// Example `literal`
// Example `_`
// Example `identifier`
pub fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let token = parser.consume()?;

    let parselet = parser.prefix_pattern_parselets.get(&token.kind);
    if parselet.is_none() {
        return Err(ParseError::UnexpectedToken(
            token,
            "invalid pattern prefix".to_string(),
        ));
    }

    let prefix = parselet.unwrap().clone();
    let mut left = prefix.parse(parser, token)?;

    loop {
        match parser.lookahead(0)?.kind {
            TokenKind::OpenBrace | TokenKind::Equal => break,
            _ => (),
        };

        let next_token = parser.consume()?;

        let parselet = parser.infix_pattern_parselets.get(&next_token.kind);
        if parselet.is_none() {
            return Err(ParseError::UnexpectedToken(
                next_token.clone(),
                "invalid infix pattern".to_string(),
            ));
        }

        let infix = parselet.unwrap().clone();
        left = infix.parse(parser, left, next_token)?;
    }

    Ok(left)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{IdentifierPat, Pattern, PayloadPat};
    use crate::parser::lexer::Lexer;
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_patterns {
        ( $( $case:ident { input: $input:expr, want: $want:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);
                    let pattern = parse_pattern(&mut parser).unwrap();
                    assert_eq!(pattern, $want)
                }
            )*
        }
    }

    test_parse_patterns!(
        test_parse_pattern_int_literal {
            input: "42 {",
            want: Pattern::IntLiteral(42),
        },
        test_parse_pattern_string_literal {
            input: r###""hello" {"###,
            want: Pattern::StringLiteral("hello".to_string()),
        },
        test_parse_pattern_float_literal {
            input: "3.45 {",
            want: Pattern::FloatLiteral(3.45),
        },
        test_parse_pattern_true_literal {
            input: "true {",
            want: Pattern::BoolLiteral(true),
        },
        test_parse_pattern_false_literal {
            input: "false {",
            want: Pattern::BoolLiteral(false),
        },
        test_parse_pattern_default {
            input: "_ =",
            want: Pattern::Default,
        },
        test_parse_pattern_identifier {
            input: "my_var =",
            want: Pattern::Identifier(IdentifierPat {
                name: "my_var".to_string()
            }),
        },
        test_parse_pattern_type_match {
            input: "f32(f) =",
            want: Pattern::Payload(PayloadPat {
                pat: Box::new(Pattern::Identifier(IdentifierPat {
                    name: "f32".to_string(),
                })),
                payload: "f".to_string(),
            }),
        },
    );

    #[test]
    fn test_parse_pattern_invalid() {
        let lexer = Lexer::new("+ ");
        let mut parser = Parser::new(lexer);
        let result = parse_pattern(&mut parser);
        assert!(result.is_err());
    }
}
