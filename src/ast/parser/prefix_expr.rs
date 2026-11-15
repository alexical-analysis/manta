use crate::ast::ast::{Expr, ExprValue, IdentifierExpr};
use crate::ast::lexer::{Lexer, Token, Ty};
use crate::ast::parser::Parser;

fn parse_identifier_expr(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Expr {
    let is_mod = lexer.peek();
    if is_mod.ty != Ty::ModuleOperator {
        return parser.get_expr(
            token,
            ExprValue::Identifier(IdentifierExpr {
                module: None,
                name: token.lexeme,
            }),
        );
    }

    lexer.next(parser.str_store);
    let ident_name = lexer.next(parser.str_store);
    if ident_name.ty != Ty::Identifier {
        lexer.recover_until_expr(parser.str_store);

        return parser.get_expr(ident_name, ExprValue::Invalid(""));
    }

    parser.get_expr(
        token,
        ExprValue::Identifier(IdentifierExpr {
            module: Some(token.lexeme),
            name: (ident_name.lexeme),
        }),
    )
}

fn parse_int_literal(parser: &mut Parser, token: Token) -> Expr {
    let s = parser.get_str(token.lexeme).replace('_', "");
    match s.parse::<i64>() {
        Ok(n) => parser.get_expr(token, ExprValue::IntLiteral(n)),
        Err(_) => parser.get_expr(token, ExprValue::Invalid("invalid integer literal")),
    }
}

fn parse_float_literal(parser: &mut Parser, token: Token) -> Expr {
    let s = parser.get_str(token.lexeme).replace('_', "");
    match s.parse::<f64>() {
        Ok(n) => parser.get_expr(token, ExprValue::FloatLiteral(n)),
        Err(_) => parser.get_expr(token, ExprValue::Invalid("invalid float literal")),
    }
}

pub fn parse_prefix_expr(parser: &mut Parser, lexer: &mut Lexer, token: Token) -> Expr {
    match token.ty {
        Ty::Identifier => parse_identifier_expr(parser, lexer, token),
        Ty::BreakKeyword => parser.get_expr(token, ExprValue::Break),
        Ty::Int => parse_int_literal(parser, token),
        Ty::Float => parse_float_literal(parser, token),
        Ty::TrueKeyword => parser.get_expr(token, ExprValue::BoolLiteral(true)),
        Ty::FalseKeyword => parser.get_expr(token, ExprValue::BoolLiteral(false)),
        _ => todo!("parse_prefix_expr"),
    }
}
