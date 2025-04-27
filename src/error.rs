use colored::{Color, Colorize};

use std::cmp::max;
use std::fmt;

use crate::syntax::{Span, Token};

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
    pub token: Token,
    pub source_line: String,
}

impl ParseError {
    pub fn new(
        message: impl Into<String>,
        filename: &str,
        token: &Token,
        source_line: &str,
    ) -> Self {
        ParseError {
            filename: filename.to_string(),
            message: message.into(),
            token: token.clone(),
            source_line: source_line.to_string(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            report_error(
                "📍 Syntax error",
                &self.message,
                &Span {
                    filename: self.filename.clone(),
                    line: self.token.line,
                    column_start: self.token.column_start,
                    column: self.token.column,
                },
                &self.source_line
            )
        )
    }
}

impl std::error::Error for ParseError {}

fn report_error(label: &str, msg: &str, span: &Span, source_line: &str) -> String {
    let error_color = Color::Red;
    let location_color = Color::Cyan;
    let gutter_color = Color::Blue;

    let line_str = span.line.to_string();
    let gutter_width = line_str.len();

    let padding = span.column_start.saturating_sub(1);

    let underline_len = max(1, span.column.saturating_sub(span.column_start));

    let padding_str = " ".repeat(padding);
    let underline_str = "^".repeat(underline_len);

    format!(
        "{}: {}\n {} {}\n {:>width$} {} {}\n {:>width$} {} {}{}",
        label.color(error_color).bold(),
        msg,
        "→".normal(),
        format!("{}:{}:{}", &span.filename, span.line, span.column_start).color(location_color),
        span.line.to_string().color(gutter_color),
        "|".color(gutter_color),
        source_line.trim_end(),
        "",
        "|".color(gutter_color),
        &padding_str,
        &underline_str.color(error_color).bold(),
        width = gutter_width
    )
}
