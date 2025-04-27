#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    Plus,         // +
    Minus,        // -
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
    Equal,        // =
    Bang,         // !
    Less,         // <
    Greater,      // >
    Question,     // ?

    // Compound character tokens
    StarStar,      // **
    DotDot,        // ..
    ColonColon,    // ::
    PlusEqual,     // +=
    MinusEqual,    // -=
    StarEqual,     // *=
    StarStarEqual, // **=
    SlashEqual,    // /=
    PercentEqual,  // %=
    Arrow,         // ->
    EqualEqual,    // ==
    BangEqual,     // !=
    LessEqual,     // <=
    GreaterEqual,  // >=
    AndAnd,        // &&
    OrOr,          // ||

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

    // Native functions (or keywords acting like functions)
    Emit,

    // Meta tokens
    Error(String),
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
    pub column_start: usize,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize, column_start: usize) -> Self {
        Token {
            kind,
            line,
            column,
            column_start,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::StarStar => write!(f, "**"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::PlusEqual => write!(f, "+="),
            TokenKind::MinusEqual => write!(f, "-="),
            TokenKind::StarEqual => write!(f, "*="),
            TokenKind::StarStarEqual => write!(f, "**="),
            TokenKind::SlashEqual => write!(f, "/="),
            TokenKind::PercentEqual => write!(f, "%="),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::AndAnd => write!(f, "&&"),
            TokenKind::OrOr => write!(f, "||"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Identifier(s) => write!(f, "{}", s),
            TokenKind::Number(n) => write!(f, "{}", n),
            TokenKind::String(s) => write!(f, "{}", s),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Constellation => write!(f, "constellation"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Switch => write!(f, "switch"),
            TokenKind::Case => write!(f, "case"),
            TokenKind::Default => write!(f, "default"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::Refraction => write!(f, "refraction"),
            TokenKind::Radiate => write!(f, "radiate"),
            TokenKind::Facet => write!(f, "facet"),
            TokenKind::Interface => write!(f, "interface"),
            TokenKind::Import => write!(f, "import"),
            TokenKind::As => write!(f, "as"),
            TokenKind::Emit => write!(f, "emit"),
            TokenKind::Error(s) => write!(f, "{}", s),
            TokenKind::Eof => write!(f, "eof"),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}:{}", self.kind, self.line, self.column_start)
    }
}
