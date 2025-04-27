use super::token::{Token, TokenKind};

pub struct Lexer<'a> {
    filename: String,
    /// Iterator over the characters of the source code.
    chars: std::str::Chars<'a>,
    /// The character currently being processed (lookahead of 1).
    current: Option<char>,
    /// Current line number (1-based).
    line: usize,
    /// Current column number (1-based, position *after* current char).
    column: usize,
    /// Start column of the token currently being built (1-based, inclusive).
    /// Set after skipping whitespace before a token starts.
    column_start: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer instance.
    pub fn new(source: &'a str, filename: &str) -> Self {
        let mut chars = source.chars();

        let first = chars.next();
        Lexer {
            filename: filename.to_string(),
            chars,
            current: first,
            line: 1,
            column: 1,       // Start at column 1
            column_start: 1, // Initial start column
        }
    }

    /// Consumes the `current` character, advances the internal iterator,
    /// and updates the column position. Returns the consumed character.
    fn advance(&mut self) -> Option<char> {
        let consumed_char = self.current;
        self.current = self.chars.next();

        if consumed_char.is_some() {
            self.column += 1;
        }

        consumed_char
    }

    /// Returns the current character without consuming it (1 character lookahead).
    fn peek(&self) -> Option<char> {
        self.current
    }

    /// Returns the character *after* the current one without consuming anything (2 characters lookahead).
    fn peek_next(&self) -> Option<char> {
        // Cloning the underlying iterator is necessary to peek ahead
        // without affecting the main iterator's state.
        self.chars.clone().next()
    }

    /// Checks if the current character matches `expected`.
    /// If it matches, consumes the character via `advance()` and returns `true`.
    /// Otherwise, returns `false` without consuming.
    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance(); // Consume the character if it matches
            true
        } else {
            false
        }
    }

    /// Skips over whitespace characters (space, tab, carriage return) and comments (`//`).
    /// Handles newlines correctly by incrementing `line` and resetting `column`.
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                // Consume simple whitespace
                Some(' ' | '\t' | '\r') => {
                    self.advance();
                }
                // Consume newline and update position
                Some('\n') => {
                    self.advance();
                    self.line += 1;
                    self.column = 1; // Reset column after newline
                }
                // Check for line comments (`//`)
                Some('/') if self.peek_next() == Some('/') => {
                    // Consume the '//'
                    self.advance();
                    self.advance();
                    // Consume characters until the end of the line or EOF
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break; // Stop *before* consuming the newline itself
                        }
                        self.advance();
                    }
                }
                // TODO: Add block comment skipping (e.g., /* ... */)

                // If it's not whitespace or a comment start, stop skipping
                _ => break,
            }
        }
    }

    /// Helper function to create a `Token` with the given `TokenKind`.
    /// It uses the lexer's current position (`line`, `column`) as the *end*
    /// of the token's span and the stored `column_start` as the beginning.
    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(
            kind,
            self.line,
            self.column,       // Current column is the end column (exclusive)
            self.column_start, // Start column was set before tokenization began
        )
    }

    /// Creates an `Error` token kind for an unexpected character encountered during lexing.
    fn unexpected_char(&self, ch: char, suggestion: Option<&str>) -> TokenKind {
        println!("unexpected_char: {:?}", ch);
        let error_col = self.column.saturating_sub(1);
        let message = match suggestion {
            Some(s) => format!(
                "Unexpected character '{}' at {}:{}:{} — did you mean '{}'?",
                ch, self.filename, self.line, error_col, s
            ),
            None => format!(
                "Unexpected character: '{}' at {}:{}:{}",
                ch, self.filename, self.line, error_col
            ),
        };
        TokenKind::Error(message)
    }

    /// Lexes an identifier or a keyword, starting with `first_char`.
    fn make_identifier(&mut self, first_char: char) -> Token {
        let mut ident = String::new();
        ident.push(first_char);

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                ident.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        // Determine if the identifier is a keyword or a plain identifier
        // Note: Types were moved from here and are now treated as identifiers, i.e TokenKind::Identifier(Light)
        // This is an attempt to match how Go handles types; it also makes it easier to parse user-defined types.
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
            "Refraction" => TokenKind::Refraction,
            "Radiate" => TokenKind::Radiate,
            "Facet" => TokenKind::Facet,
            "interface" => TokenKind::Interface,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            _ => TokenKind::Identifier(ident),
        };

        self.make_token(kind)
    }

    /// Lexes a string literal, starting *after* the opening double quote.
    fn make_string(&mut self) -> Token {
        let mut value = String::new();

        loop {
            match self.peek() {
                Some('"') => {
                    self.advance(); // Consume the closing quote
                    break; // String finished successfully
                }
                Some('\n') => {
                    // Unterminated string - do not consume the newline
                    return self
                        .make_token(TokenKind::Error("Unterminated string literal.".into()));
                }
                Some(_) => {
                    // TODO: Implement escape sequence handling (e.g., \n, \t, \", \\)
                    // For now, just append the character
                    value.push(self.advance().unwrap());
                }
                None => {
                    // Reached end of file before finding closing quote
                    return self.make_token(TokenKind::Error(
                        "Unterminated string literal (reached end of file).".into(),
                    ));
                }
            }
        }
        self.make_token(TokenKind::String(value))
    }

    /// Lexes a number literal (always resulting in `f64`), starting with `first_digit`.
    /// This function assumes `first_digit` is indeed an ASCII digit.
    /// It correctly handles decimal points, ensuring only one is allowed and
    /// that it must be followed by a digit to be consumed as part of the number.
    /// Stops consuming before a dot if it's not followed by a digit (e.g., for `1.`).
    fn make_number(&mut self, first_digit: char) -> Token {
        let mut buf = String::new();
        buf.push(first_digit);

        // Flag to ensure only one decimal point is consumed
        let mut seen_dot = false;

        // Consume subsequent digits and potentially one decimal point sequence
        while let Some(c) = self.peek() {
            match c {
                // Consume digits
                d if d.is_ascii_digit() => {
                    buf.push(self.advance().unwrap());
                }
                // Consume a dot ONLY if it's the first one seen AND followed by a digit
                '.' if !seen_dot && self.peek_next().map_or(false, |n| n.is_ascii_digit()) => {
                    buf.push(self.advance().unwrap());
                    seen_dot = true;
                }
                // Any other character (a second dot, dot not followed by digit, non-digit)
                // signifies the end of the number literal.
                _ => break,
            }
        }

        // Attempt to parse the collected string as f64
        match buf.parse::<f64>() {
            Ok(value) => self.make_token(TokenKind::Number(value)),
            Err(e) => {
                let error_message = format!(
                    "Invalid number format '{}': {} at {}:{}:{}",
                    buf, e, self.filename, self.line, self.column_start
                );
                self.make_token(TokenKind::Error(error_message))
            }
        }
    }

    /// Returns the next token from the input source code.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        // Record the starting column of the potential token *after* skipping whitespace
        self.column_start = self.column;

        let first_char = match self.advance() {
            Some(c) => c,
            None => return self.make_token(TokenKind::Eof),
        };

        match first_char {
            // Single-character tokens
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '[' => self.make_token(TokenKind::LeftBracket),
            ']' => self.make_token(TokenKind::RightBracket),
            ',' => self.make_token(TokenKind::Comma),
            ';' => self.make_token(TokenKind::Semicolon),
            '?' => self.make_token(TokenKind::Question),

            // Tokens that might be one or two characters
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
                } else if self.match_char('*') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::StarStarEqual)
                    } else {
                        self.make_token(TokenKind::StarStar)
                    }
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
            ':' => {
                if self.match_char(':') {
                    self.make_token(TokenKind::ColonColon)
                } else {
                    self.make_token(TokenKind::Colon)
                }
            }
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
                    self.make_token(self.unexpected_char(first_char, Some("||")))
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.make_token(TokenKind::AndAnd)
                } else {
                    self.make_token(self.unexpected_char(first_char, Some("&&")))
                }
            }

            // String literals start with "
            // TODO: Add single quote (') to start a character literal or Unicode-point (e.g. 'a') like in Go
            '"' => self.make_string(),

            '.' => {
                if self.match_char('.') {
                    self.make_token(TokenKind::DotDot)
                } else {
                    self.make_token(TokenKind::Dot)
                }
            }

            // Numbers start with a digit
            // TODO: handle hex, octal, and binary literals (e.g. 0xFFF, 0o123, 0b1010)
            // TODO: handle scientific notation (e.g. 1.23e-4)
            // TODO: handle underscores as separators for better readability (e.g. 1_000_000)
            c if c.is_ascii_digit() => self.make_number(c),

            // Identifiers or keywords start with a letter or underscore
            // TODO: Maybe move @ to a separate tokenKind (decorators?)
            c if c.is_ascii_alphabetic() || c == '_' || c == '@' => self.make_identifier(c),

            // Any other character is unexpected
            other => self.make_token(self.unexpected_char(other, None)),
        }
    }
}
