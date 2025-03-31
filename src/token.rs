#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Equal,      // =
    Colon,      // :
    Semicolon,  // ;
    Comma,      // ,
    Dot,        // .
    LeftParen,  // (
    RightParen, // )
    LeftBrace,  // {
    RightBrace, // }

    // One or two character tokens
    EqualEqual,   // ==
    BangEqual,    // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    Question,     // ?

    // Literals
    Identifier(String),
    Number(f64),
    String(String),

    // Keywords
    Let,
    Const,
    Fn,
    Return,
    If,
    Else,
    For,
    While,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    True,
    False,
    Umbra,
    Light,
    Lumens,
    Type,
    Struct,
    Interface,
    Import,
    As,

    // End of file
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}
