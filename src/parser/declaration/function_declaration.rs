use crate::ast::{Decl, FunctionDecl, FunctionType, Parameter, TypeSpec};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Pos, Token, Ty};
use crate::parser::types;
use crate::str_store::{StrID, StrStore};

/// Parses top-level function declarations
///
/// Example: `fn add(a, b i32) i32 { return a + b }`
pub struct FunctionDeclParselet {
    pub public: bool,
}

impl DeclParselet for FunctionDeclParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Decl, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let next = lexer.next(&mut str_store);
        if next.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected function name".to_string(),
            ));
        }

        let name = next.lexeme;

        // Expect opening paren
        let next = lexer.next(&mut str_store);
        if next.ty != Ty::OpenParen {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected '(' after function name".to_string(),
            ));
        }

        let parsed_params = parse_parameters(lexer)?;

        // Expect closing paren
        let next = lexer.next(&mut str_store);
        if next.ty != Ty::CloseParen {
            return Err(ParseError::UnexpectedToken(
                next,
                "Expected ')' after parameters".to_string(),
            ));
        }

        // Parse optional return type
        let return_type = if lexer.peek().ty == Ty::OpenBrace {
            Box::new(TypeSpec::Unit)
        } else {
            let next = lexer.next(&mut str_store);
            let t = types::parse_type(lexer, next)?;
            Box::new(t)
        };

        // Parse function body
        let next = lexer.next(&mut str_store);
        if next.ty != Ty::OpenBrace {
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
            public: self.public,
            id: token.pos,
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
    id: Pos,
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
    if lexer.peek().ty == Ty::CloseParen {
        return Ok(params);
    }

    todo!("need to figure this out");
    let mut str_store = StrStore::new();

    loop {
        // Collect parameter names
        let mut param_tokens = vec![];

        // Get first identifier
        let ident_token = lexer.next(&mut str_store);
        if ident_token.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident_token,
                "Expected parameter name".to_string(),
            ));
        }

        param_tokens.push(ident_token);

        // Keep collecting params separated by commas while we see: comma, identifier, comma/paren/type
        loop {
            if lexer.peek().ty != Ty::Comma {
                // this should be a type so we can stop collecting params
                break;
            }
            lexer.next(&mut str_store);

            let ident_token = lexer.next(&mut str_store);
            if ident_token.ty != Ty::Identifier {
                return Err(ParseError::UnexpectedToken(
                    ident_token,
                    "Expected parameter name".to_string(),
                ));
            }

            param_tokens.push(ident_token);
        }

        // Now parse the type spec
        let type_token = lexer.next(&mut str_store);
        let type_spec = types::parse_type(lexer, type_token)?;

        // Add all parameters with this type
        for token in param_tokens {
            params.push(ParsedParam {
                id: token.pos,
                name: token.lexeme,
                type_spec: type_spec.clone(),
            });
        }

        // Check if there are more parameters
        match lexer.peek().ty {
            Ty::CloseParen => break,
            Ty::Comma => lexer.next(&mut str_store),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    lexer.peek().clone(),
                    "Expected ',' or ')' in parameter list".to_string(),
                ));
            }
        };
    }

    Ok(params)
}
