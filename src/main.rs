#![allow(dead_code)] // TODO: Remove this
mod lexer;
mod token;

use crate::lexer::Lexer;

fn main() {
    let code = r#"
        // This is a comment
        // another comment

        let x: Light = 42;

        const y = x + 3.14;

        fn add(a: Light, b: Light) -> Light {}
    "#;

    let mut lexer = Lexer::new(code);

    loop {
        let token = lexer.next_token();
        println!("{:?}", token);
        if matches!(token.kind, crate::token::TokenKind::EOF) {
            break;
        }
    }
}
