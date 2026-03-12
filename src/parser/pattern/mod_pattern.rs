use crate::ast::{ModuleIdentifierPat, NamedType, Pattern, TypeSpec, TypeSpecPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser};

/// Parses module access patterns
///
/// Example: `math::Vec3(_) or io::Result`
pub struct ModPatternParselet;

impl InfixPatternParselet for ModPatternParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        let module = match left {
            Pattern::Identifier(ident) => ident,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "module name must be an Identifier".to_string(),
                ));
            }
        };

        let ident = lexer.next_token();
        if ident.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident,
                "expected identifier in module access to be an identifier".to_string(),
            ));
        }

        let mut payload = None;
        if lexer.peek().kind == TokenKind::OpenParen {
            lexer.next_token();
            let payload_token = lexer.next_token();
            if payload_token.kind != TokenKind::Identifier {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "invalid payload for enum constructor".to_string(),
                ));
            }

            let close = lexer.next_token();
            if close.kind != TokenKind::CloseParen {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "missing closing paran for pattern payload".to_string(),
                ));
            }

            payload = Some(payload_token.lexeme_id)
        }

        match payload {
            // If we have a payload then this must be a type spec pattern
            Some(p) => Ok(Pattern::TypeSpec(TypeSpecPat {
                id: token.source_id,
                type_spec: TypeSpec::Named(NamedType {
                    id: module.id,
                    module: Some(module.name),
                    name: ident.lexeme_id,
                }),
                payload: p,
            })),
            // Otherwise parse this as a mod identifier
            None => Ok(Pattern::ModuleIdentifier(ModuleIdentifierPat {
                id: module.id,
                module: module.name,
                name: ident.lexeme_id,
            })),
        }
    }
}
