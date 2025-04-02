#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl ParseError {
    pub fn new(message: impl Into<String>, token: &crate::token::Token) -> Self {
        ParseError {
            message: message.into(),
            line: token.line,
            column: token.column,
        }
    }
}
