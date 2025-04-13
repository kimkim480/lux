use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    filename: String,
    chars: std::str::Chars<'a>,
    current: Option<char>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, filename: &str) -> Self {
        let mut chars = source.chars();
        let first = chars.next();
        Lexer {
            filename: filename.to_string(),
            chars,
            current: first,
            line: 1,
            column: 1,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let ch = match self.advance() {
            Some(c) => c,
            None => return self.make_token(TokenKind::Eof),
        };

        match ch {
            '+' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::PlusEqual)
                } else {
                    self.make_token(TokenKind::Plus)
                }
            }
            '*' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::StarEqual)
                } else {
                    self.make_token(TokenKind::Star)
                }
            }
            '/' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::SlashEqual)
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            '%' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::PercentEqual)
                } else {
                    self.make_token(TokenKind::Percent)
                }
            }
            ':' => self.make_token(TokenKind::Colon),
            ';' => self.make_token(TokenKind::Semicolon),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Dot),
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '[' => self.make_token(TokenKind::LeftBracket),
            ']' => self.make_token(TokenKind::RightBracket),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '"' => self.make_string(),
            '-' => {
                if self.match_char('>') {
                    self.make_token(TokenKind::Arrow)
                } else if self.match_char('=') {
                    self.make_token(TokenKind::MinusEqual)
                } else {
                    self.make_token(TokenKind::Minus)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqualEqual)
                } else {
                    self.make_token(TokenKind::Equal)
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::BangEqual)
                } else {
                    self.make_token(TokenKind::Bang)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::LessEqual)
                } else {
                    self.make_token(TokenKind::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::GreaterEqual)
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.make_token(TokenKind::OrOr)
                } else {
                    self.make_token(self.unexpected_char(ch, Some("||")))
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.make_token(TokenKind::AndAnd)
                } else {
                    self.make_token(self.unexpected_char(ch, Some("&&")))
                }
            }
            '?' => self.make_token(TokenKind::Question),
            c if c.is_ascii_digit() => self.make_number(c),
            c if c.is_ascii_alphabetic() || c == '_' => self.make_identifier(c),
            other => self.make_token(self.unexpected_char(other, None)),
        }
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.current;
        self.current = self.chars.next();
        self.column += 1;
        ch
    }

    fn is_at_end(&self) -> bool {
        self.peek().is_none()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn make_identifier(&mut self, first_char: char) -> Token {
        let mut ident = first_char.to_string();
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                ident.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        let kind = match ident.as_str() {
            "constellation" => TokenKind::Constellation,
            "emit" => TokenKind::Emit,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "Umbra" => TokenKind::Umbra,
            "Light" => TokenKind::Light,
            "Lumens" => TokenKind::Lumens,
            "Photon" => TokenKind::Photon,
            "Refraction" => TokenKind::Refraction,
            "Facet" => TokenKind::Facet,
            "interface" => TokenKind::Interface,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            _ => TokenKind::Identifier(ident),
        };

        self.make_token(kind)
    }

    fn make_number(&mut self, first_char: char) -> Token {
        let mut number = first_char.to_string();
        while let Some(n) = self.peek() {
            if n.is_ascii_digit() || n == '.' {
                number.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        let value = number.parse().unwrap_or(0.0);
        self.make_token(TokenKind::Number(value))
    }

    fn make_string(&mut self) -> Token {
        let mut value = String::new();

        while let Some(c) = self.peek() {
            if c == '"' {
                self.advance(); // consume closing quote
                break;
            } else if c == '\n' || self.is_at_end() {
                return self.make_token(TokenKind::Error("Unterminated string".into()));
            } else {
                value.push(self.advance().unwrap());
            }
        }

        self.make_token(TokenKind::String(value))
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            line: self.line,
            column: self.column,
        }
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.advance();
            } else if c == '\n' {
                self.line += 1;
                self.column = 0;
                self.advance();
            } else if c == '/' && self.peek_next() == Some('/') {
                // Skip line comment
                self.advance(); // consume first '/'
                self.advance(); // consume second '/'
                while let Some(comment_char) = self.peek() {
                    if comment_char == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn unexpected_char(&self, ch: char, suggestion: Option<&str>) -> TokenKind {
        let message = match suggestion {
            Some(s) => format!(
                "Unexpected '{}' at {}:{}:{} — you probably meant '{}'",
                ch, self.filename, self.line, self.column, s
            ),
            None => format!(
                "Unexpected character: '{}' at {}:{}:{}",
                ch, self.filename, self.line, self.column
            ),
        };

        TokenKind::Error(message)
    }
}
