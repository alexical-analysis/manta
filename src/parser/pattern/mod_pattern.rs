use crate::ast::{IdentifierPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser};

/// Parses module access patterns
///
/// Example: `math::Vec3`
pub struct ModPatternParselet;

impl InfixPatternParselet for ModPatternParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        match left {
            Pattern::Identifier(ident) => {
                let module = lexer.next_token();
                if module.kind != TokenKind::Identifier {
                    return Err(ParseError::UnexpectedToken(
                        module,
                        "expected module name to be an identifier".to_string(),
                    ));
                }

                Ok(Pattern::Identifier(IdentifierPat {
                    id: ident.id,
                    name: ident.name,
                    module: Some(module.lexeme_id),
                }))
            }
            _ => Err(ParseError::UnexpectedToken(
                token,
                "module must be an Identifier".to_string(),
            )),
        }
    }
}
