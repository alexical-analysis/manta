use crate::ast::TypeSpec;
use crate::parser::ParseError;

pub fn parse_type_placeholder() -> Result<TypeSpec, ParseError> {
    Err(ParseError::Custom(
        "type parsing not implemented yet".into(),
    ))
}
