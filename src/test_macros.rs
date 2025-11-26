// Shared test macro for parselet parse tests
// This macro is exported so test modules across the crate can reuse it.
#[macro_export]
macro_rules! test_parselet {
    ( $parselet:ident, $( $case:ident { input: $input:expr, want: $want:pat, want_value: $want_value:expr, } ),*, ) => {
        $(
            #[test]
            fn $case() {
                // Bring commonly used items into scope for the generated tests.
                use crate::parser::lexer::Lexer;
                use crate::parser::Parser;

                let mut lexer = Lexer::new($input);
                let token = lexer.next_token().unwrap();

                let mut parser = Parser::new(lexer);
                let result = $parselet.parse(&mut parser, token);

                assert!(result.is_ok());
                let result = result.unwrap();

                match result {
                    $want => $want_value,
                    _ => panic!("Expected {}, but got {:?}", stringify!($want), result),
                }
            }
        )*
    };
}
