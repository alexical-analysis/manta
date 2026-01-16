use crate::ast::{Decl, FunctionDecl, Parameter};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixDeclParselet;
use crate::parser::statement;
use crate::parser::types;
use crate::parser::{ParseError, Parser};

/// Parses top-level function declarations
///
/// Example: `fn add(a, b i32) i32 { return a + b }`
pub struct FunctionDeclParselet;

impl PrefixDeclParselet for FunctionDeclParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Decl, ParseError> {
        let func_decl = parse_function_declaration(parser)?;
        Ok(Decl::Function(func_decl))
    }
}

fn parse_function_declaration(parser: &mut Parser) -> Result<FunctionDecl, ParseError> {
    // Expect function name
    let name_token = parser.lookahead(0)?;
    if name_token.kind != TokenKind::Identifier {
        return Err(ParseError::UnexpectedToken(
            name_token.clone(),
            "Expected function name".to_string(),
        ));
    }

    let name = parser.consume()?.lexeme;

    // Expect opening paren
    let matched = parser.match_token(TokenKind::OpenParen)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected '(' after function name".to_string(),
        ));
    }

    // Parse parameters
    let params = parse_parameters(parser)?;

    // Expect closing paren
    let matched = parser.match_token(TokenKind::CloseParen)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected ')' after parameters".to_string(),
        ));
    }

    // Parse optional return type
    let return_type = if parser.lookahead(0)?.kind == TokenKind::OpenBrace {
        None
    } else {
        let token = parser.consume()?;
        Some(types::parse_type(parser, token)?)
    };

    // Parse function body
    let matched = parser.match_token(TokenKind::OpenBrace)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected '{' before function body".to_string(),
        ));
    }

    let body = statement::parse_block(parser)?;

    Ok(FunctionDecl {
        name,
        params,
        return_type,
        body,
    })
}

/// Parse function parameters
/// Syntax: param_list: param (',' param)*
/// param: identifier (type_spec)?
/// Type annotations can be shared: `a, b i32` means both a and b are i32
fn parse_parameters(parser: &mut Parser) -> Result<Vec<Parameter>, ParseError> {
    let mut params = vec![];

    // Check for empty parameter list
    if parser.lookahead(0)?.kind == TokenKind::CloseParen {
        return Ok(params);
    }

    loop {
        // Collect parameter names
        let mut param_names = vec![];

        // Get first identifier
        let tok = parser.lookahead(0)?;
        if tok.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                tok.clone(),
                "Expected parameter name".to_string(),
            ));
        }
        param_names.push(parser.consume()?.lexeme);

        // Keep collecting params separated by commas while we see: comma, identifier, comma/paren/type
        loop {
            if parser.lookahead(0)?.kind != TokenKind::Comma {
                break;
            }

            // Peek at what's after the comma
            let after_comma = parser.lookahead(1)?;
            if after_comma.kind != TokenKind::Identifier {
                // Not an identifier after comma, so type comes next
                break;
            }

            // We have comma + identifier. Consume the comma and identifier
            parser.consume()?; // comma
            param_names.push(parser.consume()?.lexeme); // identifier
        }

        // Now parse the type spec
        let token = parser.consume()?;
        let type_spec = types::parse_type(parser, token)?;

        // Add all parameters with this type
        for name in param_names {
            params.push(Parameter {
                name,
                type_spec: type_spec.clone(),
            });
        }

        // Check if there are more parameters
        match parser.lookahead(0)?.kind {
            TokenKind::CloseParen => break,
            TokenKind::Comma => {
                parser.consume()?;
                // Loop continues to parse next parameter group
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    parser.lookahead(0)?.clone(),
                    "Expected ',' or ')' in parameter list".to_string(),
                ));
            }
        }
    }

    Ok(params)
}
