use crate::ast::{ModuleAccesPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::pattern::{InfixPatternParselet, PatternParser};

/// Parses module access patterns
///
/// Example: `math::Vec3`
pub struct ModPatternParselet;

impl InfixPatternParselet for ModPatternParselet {
    fn parse(
        &self,
        parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        match left {
            Pattern::Identifier(ident) => {
                let pattern = parser.parse(lexer)?;

                Ok(Pattern::ModuleAccess(ModuleAccesPat {
                    module: Box::new(ident),
                    pat: Box::new(pattern),
                }))
            }
            _ => Err(ParseError::UnexpectedToken(
                token,
                "module must be an Identifier".to_string(),
            )),
        }
    }
}

// TODO: add tests for the parselet in the pattern.rs test suite
