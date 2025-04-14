use std::fmt;

use crate::{ast::Span, token::TokenKind};

pub type PrismResult<T> = Result<T, PrismError>;

#[derive(Debug)]
pub enum PrismError {
    Compile {
        message: String,
        span: Span,
        source_line: String,
    },
    Runtime(String),
    Type {
        message: String,
        span: Span,
        source_line: String,
    },
}

impl fmt::Display for PrismError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrismError::Compile {
                message,
                span,
                source_line,
            } => write!(
                f,
                "{}",
                report_error("💥 Compile error", message, span, source_line)
            ),
            PrismError::Runtime(msg) => write!(f, "💥 Runtime error: {}", msg),
            PrismError::Type {
                message,
                span,
                source_line,
            } => write!(
                f,
                "{}",
                report_error("🚨 Type error", message, span, source_line)
            ),
        }
    }
}

impl std::error::Error for PrismError {}

impl PrismError {
    pub fn type_error<T: ToString>(message: T, span: &Span, source_line: &str) -> Self {
        PrismError::Type {
            message: message.to_string(),
            span: span.clone(),
            source_line: source_line.to_string(),
        }
    }

    pub fn compile_error<T: ToString>(message: T, span: &Span, source_line: &str) -> Self {
        PrismError::Compile {
            message: message.to_string(),
            span: span.clone(),
            source_line: source_line.to_string(),
        }
    }
}
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

fn report_error(label: &str, msg: &str, span: &Span, source_line: &str) -> String {
    let line_str = span.line.to_string();
    let gutter_width = line_str.len();

    format!(
        "{}: {}\n → {}:{}:{}\n {:>width$} | {}\n {:>width$} | {}^",
        label,
        msg,
        span.filename,
        span.line,
        span.column,
        span.line,
        source_line,
        "",
        " ".repeat(span.column.saturating_sub(1)),
        width = gutter_width,
    )
}
