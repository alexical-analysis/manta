use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::{ParseError, Precedence};

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
