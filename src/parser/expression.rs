use super::Precedence;
use crate::ast::Expr;
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser};

/// Parse an expression with a minimum precedence level.
/// This implements the Pratt parser algorithm.
pub fn parse_expression(
    parser: &mut Parser,
    min_precedence: Precedence,
) -> Result<Expr, ParseError> {
    let token = parser.consume()?;

    let prefix_opt = parser.prefix_expr_parselets.get(&token.kind);
    if prefix_opt.is_none() {
        return Err(ParseError::UnexpectedToken(format!(
            "No prefix parselet for token kind: {:?}",
            token.kind
        )));
    };

    let prefix = prefix_opt.unwrap().clone();
    let mut left = prefix.parse(parser, token)?;

    // Loop while the next token's precedence is higher than or equal to min_precedence
    loop {
        let next_token = parser.lookahead(0)?.clone();
        if expression_done(&next_token.kind) {
            break;
        }

        let next_precedence = parser.get_precedence(&next_token.kind);
        if next_precedence.is_err() {
            break;
        }
        let next_precedence = next_precedence.unwrap();

        if next_precedence <= min_precedence {
            break;
        }

        let token = parser.consume()?;
        let infix_opt = parser.infix_expr_parselets.get(&token.kind);
        if infix_opt.is_none() {
            break;
        }
        let infix = infix_opt.unwrap().clone();

        left = infix.parse(parser, left, token)?;
    }

    Ok(left)
}

/// Check if the expression is done based on the next token kind
fn expression_done(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Eof
            | TokenKind::CloseParen
            | TokenKind::CloseSquare
            | TokenKind::CloseBrace
            | TokenKind::Comma
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::TypeSpec;
    use crate::ast::{BinaryOp, Expr, UnaryOp};
    use crate::parser::lexer::Lexer;

    macro_rules! test_parse_expressions {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);

                    let expr = parse_expression(&mut parser, Precedence::Base).unwrap();
                    match expr {
                        $want_var => {$want_value},
                        _ => panic!("Expected {:?} => {{ {:?} }}, got {:?}", stringify!($want_var), stringify!($want_value), expr),
                    }
                }
            )*
        };
    }

    test_parse_expressions!(
        parse_expression_int_literal {
            input: "42",
            want_var: Expr::IntLiteral(42),
            want_value: (),
        },
        parse_expression_float_literal {
            input: "3.14",
            want_var: Expr::FloatLiteral(3.14),
            want_value: (),
        },
        parse_expression_string_literal {
            input: r#""hello world""#,
            want_var: Expr::StringLiteral(s),
            want_value: assert_eq!(s, "hello world"),
        },
        parse_expression_bool_true {
            input: "true",
            want_var: Expr::BoolLiteral(true),
            want_value: (),
        },
        parse_expression_bool_false {
            input: "false",
            want_var: Expr::BoolLiteral(false),
            want_value: (),
        },
        parse_expression_nil_literal {
            input: "nil",
            want_var: Expr::NilLiteral,
            want_value: (),
        },
        parse_expression_identifier {
            input: "myVariable",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "myVariable"),
        },
        parse_expression_identifier_single_char {
            input: "x",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "x"),
        },
        parse_expression_negative_int {
            input: "-42",
            want_var: Expr::UnaryExpr(unary),
            want_value: {
                assert_eq!(unary.operator, UnaryOp::Negate);
                match *unary.operand {
                    Expr::IntLiteral(val) => assert_eq!(val, 42),
                    _ => panic!("Expected IntLiteral in UnaryExpr"),
                }
            },
        },
        parse_expression_positive_int {
            input: "+42",
            want_var: Expr::UnaryExpr(unary),
            want_value: {
                assert_eq!(unary.operator, UnaryOp::Positive);
                match *unary.operand {
                    Expr::IntLiteral(val) => assert_eq!(val, 42),
                    _ => panic!("Expected IntLiteral in UnaryExpr"),
                }
            },
        },
        parse_expression_positive_negative_int {
            input: "+-42",
            want_var: Expr::UnaryExpr(positive_unary),
            want_value: {
                assert_eq!(positive_unary.operator, UnaryOp::Positive);
                match *positive_unary.operand {
                    Expr::UnaryExpr(negative_unary) => {
                        assert_eq!(negative_unary.operator, UnaryOp::Negate);
                        match *negative_unary.operand {
                            Expr::IntLiteral(val) => assert_eq!(val, 42),
                            _ => panic!("Expected IntLiteral in inner UnaryExpr"),
                        }
                    }
                    _ => panic!("Expected UnaryExpr in outer UnaryExpr"),
                }
            },
        },
        parse_expression_not_bool {
            input: "!true",
            want_var: Expr::UnaryExpr(unary),
            want_value: {
                assert_eq!(unary.operator, UnaryOp::Not);
                match *unary.operand {
                    Expr::BoolLiteral(val) => assert_eq!(val, true),
                    _ => panic!("Expected BoolLiteral in UnaryExpr"),
                }
            },
        },
        parse_expression_dereference {
            input: "*ptr",
            want_var: Expr::UnaryExpr(unary),
            want_value: {
                assert_eq!(unary.operator, UnaryOp::Dereference);
                match *unary.operand {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "ptr"),
                    _ => panic!("Expected Identifier in UnaryExpr"),
                }
            },
        },
        parse_expression_address_of {
            input: "&var",
            want_var: Expr::UnaryExpr(unary),
            want_value: {
                assert_eq!(unary.operator, UnaryOp::AddressOf);
                match *unary.operand {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "var"),
                    _ => panic!("Expected Identifier in UnaryExpr"),
                }
            },
        },
        parse_expression_double_dereference {
            input: "**ptr",
            want_var: Expr::UnaryExpr(outer_unary),
            want_value: {
                assert_eq!(outer_unary.operator, UnaryOp::Dereference);
                match *outer_unary.operand {
                    Expr::UnaryExpr(inner_unary) => {
                        assert_eq!(inner_unary.operator, UnaryOp::Dereference);
                        match *inner_unary.operand {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "ptr"),
                            _ => panic!("Expected Identifier in inner UnaryExpr"),
                        }
                    }
                    _ => panic!("Expected UnaryExpr in outer UnaryExpr"),
                }
            },
        },
        parse_expression_negative_not {
            input: "-!x",
            want_var: Expr::UnaryExpr(negate_unary),
            want_value: {
                assert_eq!(negate_unary.operator, UnaryOp::Negate);
                match *negate_unary.operand {
                    Expr::UnaryExpr(not_unary) => {
                        assert_eq!(not_unary.operator, UnaryOp::Not);
                        match *not_unary.operand {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "x"),
                            _ => panic!("Expected Identifier in inner UnaryExpr"),
                        }
                    }
                    _ => panic!("Expected UnaryExpr in outer UnaryExpr"),
                }
            },
        },
        parse_expression_grouped_negative {
            input: "(-x)",
            want_var: Expr::UnaryExpr(negate_unary),
            want_value: {
                assert_eq!(negate_unary.operator, UnaryOp::Negate);
                match *negate_unary.operand {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "x"),
                    _ => panic!("Expected Identifier in UnaryExpr"),
                }
            },
        },
        parse_expression_negative_grouped_not {
            input: "-(!y)",
            want_var: Expr::UnaryExpr(negate_unary),
            want_value: {
                assert_eq!(negate_unary.operator, UnaryOp::Negate);
                match *negate_unary.operand {
                    Expr::UnaryExpr(not_unary) => {
                        assert_eq!(not_unary.operator, UnaryOp::Not);
                        match *not_unary.operand {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "y"),
                            _ => panic!("Expected Identifier in inner UnaryExpr"),
                        }
                    }
                    _ => panic!("Expected UnaryExpr in outer UnaryExpr"),
                }
            },
        },
        parse_expression_addition {
            input: "1 + 2",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Add);
                match *bin.left {
                    Expr::IntLiteral(1) => {}
                    _ => panic!("Expected left to be IntLiteral(1)"),
                }
                match *bin.right {
                    Expr::IntLiteral(2) => {}
                    _ => panic!("Expected right to be IntLiteral(2)"),
                }
            },
        },
        parse_expression_multiplication {
            input: "3 * 4",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Multiply);
                match *bin.left {
                    Expr::IntLiteral(3) => {}
                    _ => panic!("Expected left to be IntLiteral(3)"),
                }
                match *bin.right {
                    Expr::IntLiteral(4) => {}
                    _ => panic!("Expected right to be IntLiteral(4)"),
                }
            },
        },
        parse_expression_subtraction {
            input: "10 - 3",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Subtract);
                match *bin.left {
                    Expr::IntLiteral(10) => {}
                    _ => panic!("Expected left to be IntLiteral(10)"),
                }
                match *bin.right {
                    Expr::IntLiteral(3) => {}
                    _ => panic!("Expected right to be IntLiteral(3)"),
                }
            },
        },
        parse_expression_division {
            input: "20 / 4",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Divide);
                match *bin.left {
                    Expr::IntLiteral(20) => {}
                    _ => panic!("Expected left to be IntLiteral(20)"),
                }
                match *bin.right {
                    Expr::IntLiteral(4) => {}
                    _ => panic!("Expected right to be IntLiteral(4)"),
                }
            },
        },
        parse_expression_modulo {
            input: "10 % 3",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Modulo);
                match *bin.left {
                    Expr::IntLiteral(10) => {}
                    _ => panic!("Expected left to be IntLiteral(10)"),
                }
                match *bin.right {
                    Expr::IntLiteral(3) => {}
                    _ => panic!("Expected right to be IntLiteral(3)"),
                }
            },
        },
        parse_expression_multiply_over_add {
            input: "1 + 2 * 3",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                // Top level should be Add
                assert_eq!(bin.operator, BinaryOp::Add);

                // Left should be 1
                match *bin.left {
                    Expr::IntLiteral(1) => {}
                    _ => panic!("Expected left to be IntLiteral(1)"),
                }

                // Right should be 2 * 3
                match *bin.right {
                    Expr::BinaryExpr(right_bin) => {
                        assert_eq!(right_bin.operator, BinaryOp::Multiply);
                        match *right_bin.left {
                            Expr::IntLiteral(2) => {}
                            _ => panic!("Expected 2"),
                        }
                        match *right_bin.right {
                            Expr::IntLiteral(3) => {}
                            _ => panic!("Expected 3"),
                        }
                    }
                    _ => panic!("Expected BinaryExpr on right"),
                }
            },
        },
        parse_expression_associative_addition {
            input: "1 + 2 + 3",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Add);

                // Left should be (1 + 2)
                match *bin.left {
                    Expr::BinaryExpr(left_bin) => {
                        assert_eq!(left_bin.operator, BinaryOp::Add);
                        match *left_bin.left {
                            Expr::IntLiteral(1) => {}
                            _ => panic!("Expected 1"),
                        }
                        match *left_bin.right {
                            Expr::IntLiteral(2) => {}
                            _ => panic!("Expected 2"),
                        }
                    }
                    _ => panic!("Expected BinaryExpr on left"),
                }

                // Right should be 3
                match *bin.right {
                    Expr::IntLiteral(3) => {}
                    _ => panic!("Expected right to be IntLiteral(3)"),
                }
            },
        },
        parse_expression_equality_check {
            input: "a == b",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::Equal);

                match *bin.left {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "a"),
                    _ => panic!("Expected left to be Identifier(a)"),
                }

                match *bin.right {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "b"),
                    _ => panic!("Expected right to be Identifier(b)"),
                }
            },
        },
        parse_expression_inequality_check {
            input: "x != y",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::NotEqual);

                match *bin.left {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "x"),
                    _ => panic!("Expected left to be Identifier(x)"),
                }

                match *bin.right {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "y"),
                    _ => panic!("Expected right to be Identifier(y)"),
                }
            },
        },
        parse_expression_less_than {
            input: "4 < b",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::LessThan);

                match *bin.left {
                    Expr::IntLiteral(4) => (),
                    _ => panic!("Expected left to be IntLiteral(4)"),
                }

                match *bin.right {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "b"),
                    _ => panic!("Expected right to be Identifier(b)"),
                }
            },
        },
        parse_expression_greater_than {
            input: "x > y",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::GreaterThan);

                match *bin.left {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "x"),
                    _ => panic!("Expected left to be Identifier(x)"),
                }

                match *bin.right {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "y"),
                    _ => panic!("Expected right to be Identifier(y)"),
                }
            },
        },
        parse_expression_less_or_equal {
            input: "a <= b",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::LessThanOrEqual);

                match *bin.left {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "a"),
                    _ => panic!("Expected left to be Identifier(a)"),
                }

                match *bin.right {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "b"),
                    _ => panic!("Expected right to be Identifier(b)"),
                }
            },
        },
        parse_expression_greater_or_equal {
            input: "x >= 9",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::GreaterThanOrEqual);

                match *bin.left {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "x"),
                    _ => panic!("Expected left to be Identifier(x)"),
                }

                match *bin.right {
                    Expr::IntLiteral(9) => (),
                    _ => panic!("Expected right to be IntLiteral(9)"),
                }
            },
        },
        parse_expression_bool_expression {
            input: "3.14 == b && true != d",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::LogicalAnd);

                match *bin.left {
                    Expr::BinaryExpr(left_bin) => {
                        assert_eq!(left_bin.operator, BinaryOp::Equal);

                        match *left_bin.left {
                            Expr::FloatLiteral(val) => assert_eq!(val, 3.14),
                            _ => panic!("Expected FloatLiteral(3.14)"),
                        }

                        match *left_bin.right {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "b"),
                            _ => panic!("Expected Identifier(b)"),
                        }
                    }
                    _ => panic!("Expected left to be BinaryExpr"),
                }

                match *bin.right {
                    Expr::BinaryExpr(right_bin) => {
                        assert_eq!(right_bin.operator, BinaryOp::NotEqual);

                        match *right_bin.left {
                            Expr::BoolLiteral(val) => assert_eq!(val, true),
                            _ => panic!("Expected BoolLiteral(true)"),
                        }

                        match *right_bin.right {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "d"),
                            _ => panic!("Expected Identifier(d)"),
                        }
                    }
                    _ => panic!("Expected right to be BinaryExpr"),
                }
            },
        },
        parse_expression_or_over_and {
            input: "1 | 2 & b",
            want_var: Expr::BinaryExpr(bin),
            want_value: {
                assert_eq!(bin.operator, BinaryOp::BitwiseOr);

                match *bin.left {
                    Expr::IntLiteral(1) => (),
                    _ => panic!("Expected left to be IntLiteral(1)"),
                }

                match *bin.right {
                    Expr::BinaryExpr(right_bin) => {
                        assert_eq!(right_bin.operator, BinaryOp::BitwiseAnd);

                        match *right_bin.left {
                            Expr::IntLiteral(2) => (),
                            _ => panic!("Expected IntLiteral(2)"),
                        }

                        match *right_bin.right {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "b"),
                            _ => panic!("Expected Identifier(b)"),
                        }
                    }
                    _ => panic!("Expected right to be BinaryExpr"),
                }
            },
        },
        parse_expression_call_no_args {
            input: "print()",
            want_var: Expr::Call(call),
            want_value: {
                match *call.func {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "print"),
                    _ => panic!("Expected func to be Identifier(print)"),
                }
                assert_eq!(call.args.len(), 0);
            },
        },
        parse_expression_call_with_args {
            input: "sum(1, b, 3)",
            want_var: Expr::Call(call),
            want_value: {
                match *call.func {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "sum"),
                    _ => panic!("Expected func to be Identifier(sum)"),
                }
                assert_eq!(call.args.len(), 3);
                match &call.args[0] {
                    Expr::IntLiteral(1) => {}
                    _ => panic!("Expected first arg to be IntLiteral(1)"),
                }
                match &call.args[1] {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "b"),
                    _ => panic!("Expected second arg to be Identifier(b)"),
                }
                match &call.args[2] {
                    Expr::IntLiteral(3) => {}
                    _ => panic!("Expected third arg to be IntLiteral(3)"),
                }
            },
        },
        parse_expression_index_access {
            input: "arr[5]",
            want_var: Expr::Index(index),
            want_value: {
                match *index.target {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "arr"),
                    _ => panic!("Expected target to be Identifier(arr)"),
                }
                match *index.index {
                    Expr::IntLiteral(5) => (),
                    _ => panic!("Expected index to be IntLiteral(5)"),
                }
            },
        },
        parse_expression_index_access_complex {
            input: "matrix[i + 1]",
            want_var: Expr::Index(index),
            want_value: {
                match *index.target {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "matrix"),
                    _ => panic!("Expected target to be Identifier(matrix)"),
                }
                match *index.index {
                    Expr::BinaryExpr(bin) => {
                        assert_eq!(bin.operator, BinaryOp::Add);
                        match *bin.left {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "i"),
                            _ => panic!("Expected left to be Identifier(i)"),
                        }
                        match *bin.right {
                            Expr::IntLiteral(1) => (),
                            _ => panic!("Expected right to be IntLiteral(1)"),
                        }
                    }
                    _ => panic!("Expected index to be BinaryExpr"),
                }
            },
        },
        parse_expression_index_access_nested {
            input: "data[rows[i]]",
            want_var: Expr::Index(index),
            want_value: {
                match *index.target {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "data"),
                    _ => panic!("Expected target to be Identifier(data)"),
                }
                match *index.index {
                    Expr::Index(nested_index) => {
                        match *nested_index.target {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "rows"),
                            _ => panic!("Expected nested target to be Identifier(rows)"),
                        }
                        match *nested_index.index {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "i"),
                            _ => panic!("Expected nested index to be Identifier(i)"),
                        }
                    }
                    _ => panic!("Expected index to be IndexExpr"),
                }
            },
        },
        parse_expression_index_access_3d {
            input: "tensor[x][y][z]",
            want_var: Expr::Index(index),
            want_value: {
                match *index.target {
                    Expr::Index(second_index) => {
                        match *second_index.target {
                            Expr::Index(first_index) => {
                                match *first_index.target {
                                    Expr::Identifier(ident) => assert_eq!(ident.name, "tensor"),
                                    _ => panic!("Expected first target to be Identifier(tensor)"),
                                }
                                match *first_index.index {
                                    Expr::Identifier(ident) => assert_eq!(ident.name, "x"),
                                    _ => panic!("Expected first index to be Identifier(x)"),
                                }
                            }
                            _ => panic!("Expected second target to be IndexExpr"),
                        }
                        match *second_index.index {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "y"),
                            _ => panic!("Expected second index to be Identifier(y)"),
                        }
                    }
                    _ => panic!("Expected target to be IndexExpr"),
                }
                match *index.index {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "z"),
                    _ => panic!("Expected index to be Identifier(z)"),
                }
            },
        },
        parse_expression_field_access {
            input: "person.name",
            want_var: Expr::FieldAccess(field_access),
            want_value: {
                match *field_access.target {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "person"),
                    _ => panic!("Expected target to be Identifier(person)"),
                }
                assert_eq!(field_access.field.name, "name");
            },
        },
        parse_expression_field_access_nested {
            input: "company.ceo.name",
            want_var: Expr::FieldAccess(field_access),
            want_value: {
                match *field_access.target {
                    Expr::FieldAccess(nested_field_access) => {
                        match *nested_field_access.target {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "company"),
                            _ => panic!("Expected nested target to be Identifier(company)"),
                        }
                        assert_eq!(nested_field_access.field.name, "ceo");
                    }
                    _ => panic!("Expected target to be FieldAccessExpr"),
                }
                assert_eq!(field_access.field.name, "name");
            },
        },
        parse_expression_field_access_index {
            input: "users[0].email",
            want_var: Expr::FieldAccess(field_access),
            want_value: {
                match *field_access.target {
                    Expr::Index(index) => {
                        match *index.target {
                            Expr::Identifier(ident) => assert_eq!(ident.name, "users"),
                            _ => panic!("Expected index target to be Identifier(users)"),
                        }
                        match *index.index {
                            Expr::IntLiteral(0) => (),
                            _ => panic!("Expected index to be IntLiteral(0)"),
                        }
                    }
                    _ => panic!("Expected target to be IndexExpr"),
                }
                assert_eq!(field_access.field.name, "email");
            },
        },
        parse_expression_field_access_call {
            input: "config.getDatabase().host",
            want_var: Expr::FieldAccess(field_access),
            want_value: {
                match *field_access.target {
                    Expr::Call(call) => {
                        match *call.func {
                            Expr::FieldAccess(nested_field_access) => {
                                match *nested_field_access.target {
                                    Expr::Identifier(ident) => assert_eq!(ident.name, "config"),
                                    _ => panic!("Expected nested target to be Identifier(config)"),
                                }
                                assert_eq!(nested_field_access.field.name, "getDatabase");
                            }
                            _ => panic!("Expected func to be FieldAccessExpr"),
                        }
                        assert_eq!(call.args.len(), 0);
                    }
                    _ => panic!("Expected target to be CallExpr"),
                }
                assert_eq!(field_access.field.name, "host");
            },
        },
        parse_expression_call_new {
            input: "new(i32)",
            want_var: Expr::New(new_expr),
            want_value: {
                match new_expr.type_spec {
                    TypeSpec::Int32 => (),
                    _ => panic!("Expected TypeSpec::Int32, but got {:?}", new_expr.type_spec),
                }
                assert!(new_expr.len.is_none());
                assert!(new_expr.cap.is_none());
            },
        },
        parse_expression_call_new_slice {
            input: "new([]f64, 10)",
            want_var: Expr::New(new_expr),
            want_value: {
                match new_expr.type_spec {
                    TypeSpec::Slice(t) => assert_eq!(*t, TypeSpec::Float64),
                    _ => panic!("Expected TypeSpec::Slice, but got {:?}", new_expr.type_spec),
                }
                match new_expr.len {
                    Some(len_expr) => match *len_expr {
                        Expr::IntLiteral(10) => (),
                        _ => panic!("Expected len to be IntLiteral(10)"),
                    },
                    None => panic!("Expected len to be Some"),
                }
                assert!(new_expr.cap.is_none());
            },
        },
        parse_expression_call_new_slice_with_cap {
            input: "new([ ] bool, 10, 20)",
            want_var: Expr::New(new_expr),
            want_value: {
                match new_expr.type_spec {
                    TypeSpec::Slice(t) => assert_eq!(*t, TypeSpec::Bool),
                    _ => panic!(
                        "Expected TypeSpec::Slice(TypeSpec::Bool), but got {:?}",
                        new_expr.type_spec
                    ),
                }
                match new_expr.len {
                    Some(len_expr) => match *len_expr {
                        Expr::IntLiteral(10) => (),
                        _ => panic!("Expected len to be IntLiteral(10)"),
                    },
                    None => panic!("Expected len to be Some"),
                }
                match new_expr.cap {
                    Some(cap_expr) => match *cap_expr {
                        Expr::IntLiteral(20) => (),
                        _ => panic!("Expected cap to be IntLiteral(20)"),
                    },
                    None => panic!("Expected cap to be Some"),
                }
            },
        },
        parse_expression_call_new_method {
            input: "new(i32).method()",
            want_var: Expr::Call(call),
            want_value: {
                match *call.func {
                    Expr::FieldAccess(field_access) => {
                        match *field_access.target {
                            Expr::New(new_expr) => {
                                match new_expr.type_spec {
                                    TypeSpec::Int32 => (),
                                    _ => panic!(
                                        "Expected TypeSpec::Int32, but got {:?}",
                                        new_expr.type_spec
                                    ),
                                }
                                assert!(new_expr.len.is_none());
                                assert!(new_expr.cap.is_none());
                            }
                            _ => panic!("Expected target to be NewExpr"),
                        }
                        assert_eq!(field_access.field.name, "method");
                    }
                    _ => panic!("Expected func to be FieldAccessExpr"),
                }
                assert_eq!(call.args.len(), 0);
            },
        },
        parse_expression_call_free {
            input: "free(ptr)",
            want_var: Expr::Free(call),
            want_value: {
                match *call.expr {
                    Expr::Identifier(ident) => assert_eq!(ident.name, "ptr"),
                    _ => panic!("Expected arg to be Identifier(ptr)"),
                }
            },
        },
        parse_expression_call_free_complex {
            input: "free(data.buffer.ptr)",
            want_var: Expr::Free(call),
            want_value: {
                match *call.expr {
                    Expr::FieldAccess(field_access) => {
                        match *field_access.target {
                            Expr::FieldAccess(nested_field_access) => {
                                match *nested_field_access.target {
                                    Expr::Identifier(ident) => assert_eq!(ident.name, "data"),
                                    _ => panic!("Expected nested target to be Identifier(data)"),
                                }
                                assert_eq!(nested_field_access.field.name, "buffer");
                            }
                            _ => panic!(
                                "Expected target to be FieldAccessExpr, got {:?}",
                                field_access.target
                            ),
                        }
                        assert_eq!(field_access.field.name, "ptr");
                    }
                    _ => panic!("Expected arg to be FieldAccessExpr, got {:?}", call.expr),
                }
            },
        },
    );
}
