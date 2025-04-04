use std::fmt;

use crate::token::TokenKind;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub kind: TokenKind,
}

impl ParseError {
    pub fn new(message: impl Into<String>, token: &crate::token::Token) -> Self {
        ParseError {
            message: message.into(),
            line: token.line,
            column: token.column,
            kind: token.kind.clone(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "📍 Syntax error at {:?} {}:{} → {}",
            self.kind, self.line, self.column, self.message
        )
    }
}
