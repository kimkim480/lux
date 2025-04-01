use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    current: Option<char>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut chars = source.chars();
        let first = chars.next();
        Lexer {
            chars,
            current: first,
            line: 1,
            column: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let ch = match self.advance() {
            Some(c) => c,
            None => return self.make_token(TokenKind::EOF),
        };

        match ch {
            '+' => self.make_token(TokenKind::Plus),
            '*' => self.make_token(TokenKind::Star),
            '/' => self.make_token(TokenKind::Slash),
            '%' => self.make_token(TokenKind::Percent),
            ':' => self.make_token(TokenKind::Colon),
            ';' => self.make_token(TokenKind::Semicolon),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Dot),
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '"' => self.make_string(),
            '-' => {
                if self.match_char('>') {
                    self.make_token(TokenKind::Arrow)
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
            "typedef" => TokenKind::Typedef,
            "struct" => TokenKind::Struct,
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
                let mut c_opt: Option<char> = self.peek();
                while c_opt != Some('\n') && !self.is_at_end() {
                    c_opt = self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn unexpected_char(&self, ch: char, suggestion: Option<&str>) -> TokenKind {
        let message = match suggestion {
            Some(s) => format!("Unexpected '{}', you probably meant '{}'", ch, s),
            None => format!("Unexpected character: '{}'", ch),
        };

        TokenKind::Error(message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    fn lex_all(source: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            tokens.push(token.kind.clone());

            if matches!(token.kind, TokenKind::EOF) {
                break;
            }
        }

        tokens
    }

    #[test]
    fn test_basic_tokens() {
        let source = "let x = 42;";
        let tokens = lex_all(source);
        assert_eq!(
            tokens,
            vec![
                TokenKind::Let,
                TokenKind::Identifier("x".into()),
                TokenKind::Equal,
                TokenKind::Number(42.0),
                TokenKind::Semicolon,
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_two_char_tokens() {
        let source = "x == y != z <= w >= 5";
        let tokens = lex_all(source);
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier("x".into()),
                TokenKind::EqualEqual,
                TokenKind::Identifier("y".into()),
                TokenKind::BangEqual,
                TokenKind::Identifier("z".into()),
                TokenKind::LessEqual,
                TokenKind::Identifier("w".into()),
                TokenKind::GreaterEqual,
                TokenKind::Number(5.0),
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_string_literal() {
        let source = r#"let greeting = "Hello, Lux!";"#;
        let tokens = lex_all(source);
        assert_eq!(
            tokens,
            vec![
                TokenKind::Let,
                TokenKind::Identifier("greeting".into()),
                TokenKind::Equal,
                TokenKind::String("Hello, Lux!".into()),
                TokenKind::Semicolon,
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_unexpected_char() {
        let source = "x | y";
        let tokens = lex_all(source);
        assert_eq!(
            tokens,
            vec![
                TokenKind::Identifier("x".into()),
                TokenKind::Error("Unexpected '|', you probably meant '||'".into()),
                TokenKind::Identifier("y".into()),
                TokenKind::EOF,
            ]
        );
    }
}
