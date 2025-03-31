mod token;

use crate::token::{Token, TokenKind};

fn main() {
    let tok = Token {
        kind: TokenKind::Let,
        line: 1,
        column: 5,
    };

    println!("{:?}", tok);
}
