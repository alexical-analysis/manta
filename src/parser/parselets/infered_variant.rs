/// Parses variants expressions where the enum is inferred from context.
///
/// Example: `.Ok`
pub struct InferedVariantParselet;

impl PrefixExprParselet for InferedVariantParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        let next = parser.lookahead(0)?;
        let variant_name = match next.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "variant identifier".to_string(),
                    found: next.kind,
                    position: next.position,
                });
            }
        };
        parser.consume_token()?; // consume identifier

        Ok(Expr::InferedVariant(InferedVariantExpr { variant_name }))
    }
}
