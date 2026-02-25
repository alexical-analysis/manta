use crate::ast::{Decl, FunctionDecl, FunctionType, Parameter, TypeSpec};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, SourceID, Token, TokenKind};
use crate::parser::types;
use crate::str_store::StrID;

/// Parses top-level function declarations
///
/// Example: `fn add(a, b i32) i32 { return a + b }`
pub struct FunctionDeclParselet;

impl DeclParselet for FunctionDeclParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Decl, ParseError> {
        let next = lexer.next_token();
        if next.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected function name".to_string(),
            ));
        }

        let name = next.lexeme_id;

        // Expect opening paren
        let next = lexer.next_token();
        if next.kind != TokenKind::OpenParen {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected '(' after function name".to_string(),
            ));
        }

        let parsed_params = parse_parameters(lexer)?;

        // Expect closing paren
        let next = lexer.next_token();
        if next.kind != TokenKind::CloseParen {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected ')' after parameters".to_string(),
            ));
        }

        // Parse optional return type
        let return_type = if lexer.peek().kind == TokenKind::OpenBrace {
            None
        } else {
            let next = lexer.next_token();
            Some(Box::new(types::parse_type(lexer, next)?))
        };

        // Parse function body
        let next = lexer.next_token();
        if next.kind != TokenKind::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected '{' before function body".to_string(),
            ));
        }

        let body = parser.parse_block(lexer, next)?;

        // build the function type and params vec
        let mut params = vec![];
        let mut types = vec![];
        for p in parsed_params {
            params.push(Parameter {
                id: p.id,
                name: p.name,
            });
            types.push(p.type_spec);
        }

        Ok(Decl::Function(FunctionDecl {
            id: token.source_id,
            name,
            params,
            body,
            function_type: FunctionType {
                params: types,
                return_type,
            },
        }))
    }
}

struct ParsedParam {
    id: SourceID,
    name: StrID,
    type_spec: TypeSpec,
}

/// Parse function parameters
/// Syntax: param_list: param (',' param)*
/// param: identifier (type_spec)?
/// Type annotations can be shared: `a, b i32` means both a and b are i32
fn parse_parameters(lexer: &mut Lexer) -> Result<Vec<ParsedParam>, ParseError> {
    let mut params = vec![];

    // Check for empty parameter list
    if lexer.peek().kind == TokenKind::CloseParen {
        return Ok(params);
    }

    loop {
        // Collect parameter names
        let mut param_tokens = vec![];

        // Get first identifier
        let ident_token = lexer.next_token();
        if ident_token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident_token,
                "Expected parameter name".to_string(),
            ));
        }

        param_tokens.push(ident_token);

        // Keep collecting params separated by commas while we see: comma, identifier, comma/paren/type
        loop {
            if lexer.peek().kind != TokenKind::Comma {
                // this should be a type so we can stop collecting params
                break;
            }
            lexer.next_token();

            let ident_token = lexer.next_token();
            if ident_token.kind != TokenKind::Identifier {
                return Err(ParseError::UnexpectedToken(
                    ident_token,
                    "Expected parameter name".to_string(),
                ));
            }

            param_tokens.push(ident_token);
        }

        // Now parse the type spec
        let type_token = lexer.next_token();
        let type_spec = types::parse_type(lexer, type_token)?;

        // Add all parameters with this type
        for token in param_tokens {
            params.push(ParsedParam {
                id: token.source_id,
                name: token.lexeme_id,
                type_spec: type_spec.clone(),
            });
        }

        // Check if there are more parameters
        match lexer.peek().kind {
            TokenKind::CloseParen => break,
            TokenKind::Comma => lexer.next_token(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    lexer.peek(),
                    "Expected ',' or ')' in parameter list".to_string(),
                ));
            }
        };
    }

    Ok(params)
}
