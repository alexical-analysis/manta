use crate::ast::Block;
use crate::parser::ParseError;

pub fn parse_block_placeholder() -> Result<Block, ParseError> {
    Err(ParseError::Custom(
        "statement parsing not implemented yet".into(),
    ))
}
