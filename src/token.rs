#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    Plus,         // +
    Star,         // *
    Slash,        // /
    Percent,      // %
    Colon,        // :
    Semicolon,    // ;
    Comma,        // ,
    Dot,          // .
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }

    // One or two character tokens
    ColonColon,   // ::
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
    Radiate,
    Facet,
    Interface,
    Import,
    As,

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

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize) -> Self {
        Token { kind, line, column }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}:{}", self.kind, self.line, self.column)
    }
}
