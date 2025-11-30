use crate::ast::{Expr, Stmt};
use crate::parser::lexer::Token;
use crate::parser::{ParseError, Parser};

pub mod assign_statement;
pub mod binary_operator;
pub mod block_statement;
pub mod bool_literal;
pub mod call;
pub mod defer_statement;
pub mod field_access;
pub mod float_literal;
pub mod group;
pub mod identifier;
pub mod index;
pub mod int_literal;
pub mod let_statement;
pub mod nil_literal;
pub mod return_statement;
pub mod string_literal;
pub mod unary_operator;

pub use assign_statement::AssignParselet;
pub use binary_operator::BinaryOperatorParselet;
pub use block_statement::BlockParselet;
pub use bool_literal::BoolLiteralParselet;
pub use call::CallParselet;
pub use defer_statement::DeferParselet;
pub use field_access::FieldAccessParselet;
pub use float_literal::FloatLiteralParselet;
pub use group::GroupParselet;
pub use identifier::IdentifierParselet;
pub use index::IndexParselet;
pub use int_literal::IntLiteralParselet;
pub use let_statement::LetParselet;
pub use nil_literal::NilLiteralParselet;
pub use return_statement::ReturnParselet;
pub use string_literal::StringLiteralParselet;
pub use unary_operator::UnaryOperatorParselet;

// Operator precedence levels.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Base,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equality,
    Comparison,
    Addition,
    Multiplication,
    Exponentiation,
    Prefix,
    Call,
}

/// Trait for prefix expression parselets.
pub trait PrefixExprParselet {
    /// Parse a prefix expression given the consumed token.
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Expr, ParseError>;
}

/// Trait for infix expression parselets.
pub trait InfixExprParselet {
    /// Parse an infix expression with `left` already parsed and the consumed token.
    fn parse(
        &self,
        parser: &mut crate::parser::Parser,
        left: Expr,
        token: Token,
    ) -> Result<Expr, ParseError>;

    /// Precedence of this infix operator.
    fn precedence(&self) -> Precedence;
}

/// Trait for prefix statement parselets.
pub trait PrefixStmtParselet {
    /// Parse a prefix statement given the consumed token.
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Stmt, ParseError>;

    /// Ensures that the parselset matches the expected token stream.
    /// This function should NEVER consume tokens from the parser or things will fail.
    fn matches(&self, parser: &mut Parser) -> bool;
}
