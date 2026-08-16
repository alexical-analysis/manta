use crate::ast::{AllocExpr, CallExpr, Expr, FreeExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet, Precedence};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses function call expressions.
///
/// Example: `foo(1, 2, "bar")`
pub struct CallParselet;

impl InfixExprParselet for CallParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        let mut arguments = vec![];
        todo!("need the actual str_store");
        let str_store = StrStore::new();

        // Check for empty argument list
        let token = lexer.peek();
        if token.ty == Ty::CloseParen {
            lexer.next(&mut str_store);
            return Ok(Expr::Call(CallExpr {
                func: Box::new(left),
                args: arguments,
            }));
        }

        // Parse arguments list
        loop {
            let arg = parser.parse(lexer, Precedence::Base)?;
            arguments.push(arg);

            let next = lexer.peek();
            if next.ty != Ty::Comma {
                break;
            }
            lexer.next(&mut str_store);
        }

        // Expect a closing ')'
        let next = lexer.next(&mut str_store);
        if next.ty != Ty::CloseParen {
            return Err(ParseError::UnexpectedToken(
                next,
                "expected ')' after function arguments".to_string(),
            ));
        }

        match &left {
            Expr::Identifier(ident) => {
                let fn_name = str_store
                    .get_string(ident.name)
                    .expect("failed to get function name");

                // TODO: maybe just check against str_id here instead of doing string comparisons
                if fn_name == "free" {
                    if arguments.len() != 1 {
                        return Err(ParseError::InvalidArguments(
                            token.clone(),
                            "free() expects exactly one argument".to_string(),
                        ));
                    }

                    let expr = arguments.remove(0);
                    let expr = Box::new(expr);
                    return Ok(Expr::Free(FreeExpr { expr }));
                }

                if fn_name == "alloc" {
                    if arguments.is_empty() {
                        return Err(ParseError::InvalidArguments(
                            token.clone(),
                            "alloc() expects at least on argument".to_string(),
                        ));
                    }

                    let expr = arguments.remove(0);
                    let expr = Box::new(expr);
                    return Ok(Expr::Alloc(AllocExpr {
                        meta_type: expr,
                        options: arguments,
                    }));
                }

                Ok(Expr::Call(CallExpr {
                    func: Box::new(left),
                    args: arguments,
                }))
            }
            _ => Ok(Expr::Call(CallExpr {
                func: Box::new(left),
                args: arguments,
            })),
        }
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
