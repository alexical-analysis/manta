use crate::ast::{ModuleAccesPat, Pattern};
use crate::parser::lexer::Token;
use crate::parser::parselets::InfixPatternParselet;
use crate::parser::{ParseError, Parser, pattern};

/// Parses module access patterns
///
/// Example: `math::Vec3`
pub struct ModPatternParselet;

impl InfixPatternParselet for ModPatternParselet {
    fn parse(
        &self,
        parser: &mut Parser,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        match left {
            Pattern::Identifier(ident) => {
                let pattern = pattern::parse_pattern(parser)?;

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
