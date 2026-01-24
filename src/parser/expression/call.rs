use crate::ast::{AllocExpr, CallExpr, Expr, FreeExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet, Precedence};
use crate::parser::lexer::{Lexer, Token, TokenKind};

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
        let mut arguments = Vec::new();

        // Check for empty argument list
        let token = lexer.peek();
        if token.kind == TokenKind::CloseParen {
            lexer.next_token();
            return Ok(Expr::Call(crate::ast::CallExpr {
                func: Box::new(left),
                args: arguments,
            }));
        }

        // Parse arguments list
        loop {
            let arg = parser.parse(lexer, Precedence::Base)?;
            arguments.push(arg);

            let next = lexer.peek();
            if next.kind != TokenKind::Comma {
                break;
            }
            lexer.next_token();
        }

        // Expect a closing ')'
        let next = lexer.next_token();
        if next.kind != TokenKind::CloseParen {
            return Err(ParseError::UnexpectedToken(
                next,
                "expected ')' after function arguments".to_string(),
            ));
        }

        match &left {
            Expr::Identifier(ident) => {
                if ident.name == "free" {
                    if arguments.len() != 1 {
                        return Err(ParseError::InvalidArguments(
                            "free() expects exactly one argument".to_string(),
                        ));
                    }

                    let expr = arguments.remove(0);
                    let expr = Box::new(expr);
                    return Ok(Expr::Free(FreeExpr { expr }));
                }

                if ident.name == "alloc" {
                    if arguments.is_empty() {
                        return Err(ParseError::InvalidArguments(
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

// TODO: add test cases here
