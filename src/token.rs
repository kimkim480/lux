#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    Plus,       // +
    Star,       // *
    Slash,      // /
    Percent,    // %
    Colon,      // :
    Semicolon,  // ;
    Comma,      // ,
    Dot,        // .
    LeftParen,  // (
    RightParen, // )
    LeftBrace,  // {
    RightBrace, // }

    // One or two character tokens
    PlusEqual,    // +=
    MinusEqual,   // -=
    StarEqual,    // *=
    SlashEqual,   // /=
    PercentEqual, // %=
    Minus,        // -
    Arrow,        // ->
    Equal,        // =
    EqualEqual,   // ==
    Bang,         // !
    BangEqual,    // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    AndAnd,       // &&
    OrOr,         // ||
    Question,     // ?

    // Literals
    Identifier(String),
    Number(f64),
    String(String),
    True,
    False,

    // Keywords
    Constellation,
    Let,
    Const,
    Fn,
    Return,
    If,
    Else,
    For,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Refraction,
    Facet,
    Interface,
    Import,
    As,

    // Native types
    Umbra,
    Light,
    Lumens,
    Photon,
    Lambda,

    // Native functions
    Emit,

    Error(String),
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}
