use crate::ast::{Decl, Expr, Pattern, Stmt};
use crate::parser::lexer::Token;
use crate::parser::{ParseError, Parser};

pub mod assign_statement;
pub mod binary_operator;
pub mod block_statement;
pub mod bool_literal;
pub mod call;
pub mod const_decl;
pub mod defer_statement;
pub mod dot_access;
pub mod dot_pattern;
pub mod float_literal;
pub mod function_declaration;
pub mod group;
pub mod identifier;
pub mod identifier_pattern;
pub mod if_statement;
pub mod index;
pub mod inferred_variant;
pub mod int_literal;
pub mod let_statement;
pub mod literal_pattern;
pub mod match_statement;
pub mod meta_type;
pub mod mod_declaration;
pub mod mod_pattern;
pub mod module_access;
pub mod payload_pattern;
pub mod return_statement;
pub mod string_literal;
pub mod type_decl;
pub mod type_pattern;
pub mod unary_operator;
pub mod use_decl;

pub use assign_statement::AssignParselet;
pub use binary_operator::BinaryOperatorParselet;
pub use block_statement::BlockParselet;
pub use bool_literal::BoolLiteralParselet;
pub use call::CallParselet;
pub use const_decl::ConstDeclParselet;
pub use defer_statement::DeferParselet;
pub use dot_access::DotAccessParselet;
pub use dot_pattern::{InfixDotPatternParselet, PrefixDotPatternParselet};
pub use float_literal::FloatLiteralParselet;
pub use function_declaration::FunctionDeclParselet;
pub use group::GroupParselet;
pub use identifier::IdentifierParselet;
pub use identifier_pattern::IdentifierPatternParselet;
pub use if_statement::IfParselet;
pub use index::IndexParselet;
pub use inferred_variant::InferedVariantParselet;
pub use int_literal::IntLiteralParselet;
pub use let_statement::LetParselet;
pub use literal_pattern::LiteralPatternParselet;
pub use match_statement::MatchParselet;
pub use meta_type::MetaTypeParselet;
pub use mod_declaration::ModDeclParselet;
pub use mod_pattern::ModPatternParselet;
pub use module_access::ModuleAccessParselet;
pub use payload_pattern::PayloadPatternParselet;
pub use return_statement::ReturnParselet;
pub use string_literal::StringLiteralParselet;
pub use type_decl::TypeDeclParselet;
pub use type_pattern::TypePatternParselet;
pub use unary_operator::UnaryOperatorParselet;
pub use use_decl::UseDeclParselet;

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
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> Result<Expr, ParseError>;

    /// Precedence of this infix operator.
    fn precedence(&self) -> Precedence;
}

/// Trait for prefix statement parselets.
pub trait PrefixStmtParselet {
    /// Parse a prefix statement given the consumed token.
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Stmt, ParseError>;
}

/// Trait for infix statement parselets.
pub trait InfixStmtParselet {
    /// Parse an infix statement with `left` already parsed and the consumed token.
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> Result<Stmt, ParseError>;
}

/// Trait for prefix pattern parselets
pub trait PrefixPatternParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Pattern, ParseError>;
}

/// Trait for infix pattern parselets.
pub trait InfixPatternParselet {
    fn parse(
        &self,
        parser: &mut Parser,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError>;
}

/// Trait for top-level declaration parselets.
pub trait PrefixDeclParselet {
    /// Parse a top-level declaration given the consumed token.
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Decl, ParseError>;
}
