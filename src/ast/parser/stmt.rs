use crate::ast::ast::{
    IfStmt, LoopStmt, MatchArm, MatchStmt, ReturnStmt, Stmt, StmtValue, WhileStmt,
};
use crate::ast::lexer::{Lexer, Token, Ty};
use crate::ast::parser::Parser;
use crate::ast::parser::infix_expr::Precedence;

fn parse_if_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let check = parser.parse_expr(lexer, Precedence::Base);

    let open_brace = lexer.next(parser.str_store);
    if open_brace.ty != Ty::OpenBrace {
        lexer.recover_until_expr(parser.str_store);
        return parser.get_expr(
            open_brace,
            StmtValue::Invalid("if expression is missing a body"),
        );
    }

    let success = parser.parse_block(lexer);

    parser.get_expr(
        token,
        StmtValue::If(IfStmt {
            check,
            success,
            fail: todo!("parse the fail branch"),
        }),
    )
}

fn parse_for_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let open_brace = lexer.next(parser.str_store);
    if open_brace.ty != Ty::OpenBrace {
        lexer.recover_until_expr(parser.str_store);
        return parser.get_expr(
            open_brace,
            StmtValue::Invalid("loop expression is missing a body"),
        );
    }

    let body = parser.parse_block(lexer);

    parser.get_expr(token, StmtValue::Loop(LoopStmt { body }))
}

fn parse_while_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let check = parser.parse_no_struct_expr(lexer);

    let open = lexer.next(parser.str_store);
    if open.ty != Ty::OpenBrace {
        lexer.recover_until_stmt(parser.str_store);
        return parser.get_stmt(
            open,
            StmtValue::Invalid("missing '{' for while statement body"),
        );
    }

    let body = parser.parse_block(lexer);
    return parser.get_stmt(token, StmtValue::While(WhileStmt { check, body }));
}

fn parse_loop_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let open = lexer.next(parser.str_store);
    if open.ty != Ty::OpenBrace {
        lexer.recover_until_stmt(parser.str_store);
        return parser.get_stmt(
            open,
            StmtValue::Invalid("loop must be followed by '{' to start loop body"),
        );
    }

    let body = parser.parse_block(lexer);
    return parser.get_stmt(token, StmtValue::Loop(LoopStmt { body }));
}

fn parse_match_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let target = parser.parse_no_struct_expr(lexer, Precedence::Base);
    let open_block = lexer.next(parser.str_store);
    if token.ty != Ty::OpenBrace {
        // this is a malformed match statement since it's missing the body
        lexer.recover_until_stmt(parser.str_store);
        return parser.get_stmt(
            token,
            StmtValue::Invalid("match statment body is missing '{'"),
        );
    }

    let mut arms = Vec::new();

    loop {
        let next_arm = lexer.peek();
        if next_arm.ty == Ty::CloseBrace {
            lexer.next(parser.str_store);
            break;
        }

        if next_arm.ty == Ty::Eof {
            lexer.recover_until_stmt(parser.str_store);
            return parser.get_stmt(
                *next_arm,
                StmtValue::Invalid("missing closing '}' in match block"),
            );
        }

        let pattern = parser.parse_pattern(lexer);

        let arm_body = lexer.next(parser.str_store);
        if arm_body.ty != Ty::OpenBrace {
            lexer.recover_until_stmt(parser.str_store);
            return parser.get_stmt(
                arm_body,
                StmtValue::Invalid("expected '{' after pattern in match arm"),
            );
        }

        let body = parser.parse_block(lexer);
        let end_body = lexer.next(parser.str_store);
        if end_body.ty != Ty::Semicolon {
            lexer.recover_until_stmt(parser.str_store);
            return parser.get_stmt(
                end_body,
                StmtValue::Invalid("expected statment end after end of match block"),
            );
        }

        arms.push(MatchArm { pattern, body })
    }

    if arms.is_empty() {
        return parser.get_stmt(
            token,
            StmtValue::Invalid("match statement must have a least one arm"),
        );
    }

    return parser.get_stmt(token, StmtValue::Match(MatchStmt { target, arms }));
}

fn parse_return_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let expr = lexer.next(parser.str_store);
    if expr.ty == Ty::Semicolon {
        // this is an empty return so we can just return here
        return parser.get_stmt(token, StmtValue::Return(ReturnStmt { value: None }));
    }

    let expr = parser.parse_expr(lexer, Precedence::Base);

    return parser.get_stmt(token, StmtValue::Return(ReturnStmt { value: Some(expr) }));
}

fn parse_block_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    let block = parser.parse_block(lexer);
    parser.get_expr(token, StmtValue::Block(block))
}

pub fn parse_stmt(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Stmt {
    match token.ty {
        Ty::IfKeyword => parse_if_stmt(parser, lexer, token),
        Ty::ForKeyword => parse_for_stmt(parser, lexer, token),
        Ty::WhileKeyword => parse_while_stmt(parser, lexer, token),
        Ty::LoopKeyword => parse_loop_stmt(parser, lexer, token),
        Ty::MatchKeyword => parse_match_stmt(parser, lexer, token),
        Ty::ReturnKeyword => parse_return_stmt(parser, lexer, token),
        Ty::OpenBrace => parse_block_stmt(parser, lexer, token),
        Ty::BreakKeyword => parser.get_stmt(token, StmtValue::Break),
        Ty::ContinueKeyword => parser.get_stmt(token, StmtValue::ContinueKeyword),
        _ => todo!("not all statments are supported yet"),
    }
}
