// Expression parsing will be implemented in later phases.
// This file is a placeholder so the module tree compiles.

use crate::ast::Expr;
use crate::parser::ParseError;

pub fn parse_expression_placeholder() -> Result<Expr, ParseError> {
    Err(ParseError::Custom(
        "expression parsing not implemented yet".into(),
    ))
}
