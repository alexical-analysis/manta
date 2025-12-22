use super::Precedence;
use crate::ast::{CallExpr, Expr, FreeExpr, IdentifierExpr, NewExpr};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::InfixExprParselet;
use crate::parser::types as type_parser;
use crate::parser::{ParseError, Parser};

/// Parses function call expressions.
///
/// Example: `foo(1, 2, "bar")`
pub struct CallParselet;

impl InfixExprParselet for CallParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> Result<Expr, ParseError> {
        // Special-case memory ops where the callee is a bare identifier
        if let Expr::Identifier(IdentifierExpr { ref name }) = left {
            if name == "new" {
                return self.parse_new_call(parser, token);
            }
        };

        // Generic function call parsing
        let mut arguments = Vec::new();

        // Check for empty argument list
        let matched = parser.match_token(TokenKind::CloseParen)?;
        if matched {
            return Ok(Expr::Call(crate::ast::CallExpr {
                func: Box::new(left),
                args: arguments,
            }));
        }

        // Parse arguments list
        loop {
            let arg = parser.parse_expression()?;
            arguments.push(arg);

            let matched = parser.match_token(TokenKind::Comma)?;
            if !matched {
                break;
            }
        }

        // Expect a closing ')'
        let matched = parser.match_token(TokenKind::CloseParen)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "expected ')' after function arguments".to_string(),
            ));
        }

        match left {
            Expr::Identifier(IdentifierExpr { ref name }) if name == "free" => {
                if arguments.len() != 1 {
                    return Err(ParseError::InvalidArguments(
                        "free() expects exactly one argument".to_string(),
                    ));
                }

                let expr = arguments.remove(0);
                let expr = Box::new(expr);
                Ok(Expr::Free(FreeExpr { expr }))
            }
            _ => Ok(Expr::Call(CallExpr {
                func: Box::new(left),
                args: arguments,
            })),
        }
    }

    fn precedence(&self) -> super::Precedence {
        Precedence::Call
    }
}

impl CallParselet {
    fn parse_new_call(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        // new(type_spec [, len [, cap]])
        // Parse a type specification first (not an expression)
        let tyspec = type_parser::parse_type(parser)?;

        let mut len = None;
        let mut cap = None;

        if parser.match_token(TokenKind::Comma)? {
            let le = parser.parse_expression()?;
            len = Some(Box::new(le));

            if parser.match_token(TokenKind::Comma)? {
                let ce = parser.parse_expression()?;
                cap = Some(Box::new(ce));
            }
        }

        if !parser.match_token(TokenKind::CloseParen)? {
            return Err(ParseError::UnexpectedToken(
                "expected ')' after new(...)".to_string(),
            ));
        }

        Ok(Expr::New(NewExpr {
            type_spec: tyspec,
            len,
            cap,
        }))
    }
}
