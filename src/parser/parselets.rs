use crate::ast::Expr;
use crate::parser::ParseError;
use crate::parser::lexer::Token;

pub mod binary_operator;
pub mod bool_literal;
pub mod float_literal;
pub mod group;
pub mod identifier;
pub mod int_literal;
pub mod nil_literal;
pub mod string_literal;
pub mod unary_operator;

pub use binary_operator::BinaryOperatorParselet;
pub use bool_literal::BoolLiteralParselet;
pub use float_literal::FloatLiteralParselet;
pub use group::GroupParselet;
pub use identifier::IdentifierParselet;
pub use int_literal::IntLiteralParselet;
pub use nil_literal::NilLiteralParselet;
pub use string_literal::StringLiteralParselet;
pub use unary_operator::UnaryOperatorParselet;

// Operator precedence levels.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Base,
    Assignment,
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

/// Trait for prefix parselets.
pub trait PrefixParselet {
    /// Parse a prefix expression given the consumed token.
    fn parse(&self, parser: &mut crate::parser::Parser, token: Token) -> Result<Expr, ParseError>;
}

/// Trait for infix parselets.
pub trait InfixParselet {
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
