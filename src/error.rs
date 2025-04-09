use std::fmt;

use crate::token::TokenKind;

pub type PrismResult<T> = Result<T, PrismError>;

#[derive(Debug)]
pub enum PrismError {
    Compile(String),
    Runtime(String),
}

impl fmt::Display for PrismError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrismError::Compile(msg) => write!(f, "💥 Compile error: {}", msg),
            PrismError::Runtime(msg) => write!(f, "💥 Runtime error: {}", msg),
        }
    }
}

impl std::error::Error for PrismError {}

#[derive(Debug)]
pub struct ParseError {
    pub filename: String,
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub kind: TokenKind,
}

impl ParseError {
    pub fn new(message: impl Into<String>, filename: &str, token: &crate::token::Token) -> Self {
        ParseError {
            filename: filename.to_string(),
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
            "📍 Syntax error at {:?} {}:{}:{} → {}",
            self.kind, self.filename, self.line, self.column, self.message
        )
    }
}

impl std::error::Error for ParseError {}
