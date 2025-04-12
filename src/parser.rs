use crate::ast::{BinaryOp, Expr, LogicalOp, Precedence, Stmt, Type, UnaryOp};
use crate::constants;
use crate::error::ParseError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Result<Expr, ParseError>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Expr) -> Result<Expr, ParseError>;

struct ParseRule<'a> {
    prefix: Option<PrefixParseFn<'a>>,
    infix: Option<InfixParseFn<'a>>,
    precedence: Precedence,
}

pub struct Parser<'a> {
    filename: String,
    previous: Token,
    current: Token,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(filename: &str, mut lexer: Lexer<'a>) -> Self {
        let first = lexer.next_token();

        Parser {
            filename: filename.to_string(),
            lexer,
            previous: first.clone(),
            current: first.clone(),
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }

    fn advance(&mut self) -> &Token {
        self.previous = self.current.clone();

        loop {
            self.current = self.lexer.next_token();
            if !matches!(self.current.kind, TokenKind::Error(_)) {
                break;
            }

            // TODO: error at current
        }

        &self.previous
    }

    fn consume_token(&mut self, expected: TokenKind, message: &str) -> Result<(), ParseError> {
        if self.match_token(&expected) {
            Ok(())
        } else {
            Err(ParseError::new(message, &self.filename, self.peek()))
        }
    }

    fn match_binary_op(&mut self) -> Option<BinaryOp> {
        let kind = &self.peek().kind;
        let op = match kind {
            TokenKind::Plus => BinaryOp::Plus,
            TokenKind::Minus => BinaryOp::Minus,
            TokenKind::Star => BinaryOp::Star,
            TokenKind::Slash => BinaryOp::Slash,
            TokenKind::EqualEqual => BinaryOp::EqualEqual,
            TokenKind::BangEqual => BinaryOp::BangEqual,
            TokenKind::Less => BinaryOp::Less,
            TokenKind::LessEqual => BinaryOp::LessEqual,
            TokenKind::Greater => BinaryOp::Greater,
            TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
            _ => return None,
        };

        self.advance();
        Some(op)
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if &self.peek().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn map_binary_op(&mut self, op: TokenKind) -> Result<BinaryOp, ParseError> {
        match op {
            TokenKind::Plus => Ok(BinaryOp::Plus),
            TokenKind::Minus => Ok(BinaryOp::Minus),
            TokenKind::Star => Ok(BinaryOp::Star),
            TokenKind::Slash => Ok(BinaryOp::Slash),
            TokenKind::Percent => Ok(BinaryOp::Percent),
            TokenKind::EqualEqual => Ok(BinaryOp::EqualEqual),
            TokenKind::BangEqual => Ok(BinaryOp::BangEqual),
            TokenKind::Less => Ok(BinaryOp::Less),
            TokenKind::LessEqual => Ok(BinaryOp::LessEqual),
            TokenKind::Greater => Ok(BinaryOp::Greater),
            TokenKind::GreaterEqual => Ok(BinaryOp::GreaterEqual),
            _ => {
                return Err(ParseError::new(
                    format!("Invalid binary operator: {:?}", op),
                    &self.filename,
                    self.peek(),
                ));
            }
        }
    }

    fn map_unary_op(&mut self, op: TokenKind) -> Result<UnaryOp, ParseError> {
        match op {
            TokenKind::Bang => Ok(UnaryOp::Bang),
            TokenKind::Minus => Ok(UnaryOp::Minus),
            _ => Err(ParseError::new(
                format!("Invalid unary operator: {:?}", op),
                &self.filename,
                self.peek(),
            )),
        }
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn parse_binary_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_expr()?;

        while let Some(op) = self.match_binary_op() {
            let right = self.parse_expr()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_emit(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume emit
        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::Semicolon, "Expected ';' after emit")?;
        Ok(Stmt::Emit(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::Semicolon, "Expected ';' after expression")?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_grouping(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::RightParen, "Expected ')' after expression")?;
        Ok(expr)
    }

    fn parse_array_literal(&mut self) -> Result<Expr, ParseError> {
        let mut elements = Vec::new();

        if !self.match_token(&TokenKind::RightBracket) {
            loop {
                let expr = self.parse_expr()?;
                elements.push(expr);

                if self.match_token(&TokenKind::RightBracket) {
                    break;
                }

                self.consume_token(TokenKind::Comma, "Expected ',' between array elements")?;
            }
        }

        Ok(Expr::Array { elements })
    }

    fn parse_index(&mut self, array: Expr) -> Result<Expr, ParseError> {
        let index = self.parse_expr()?;
        self.consume_token(TokenKind::RightBracket, "Expected ']' after index")?;

        if self.match_token(&TokenKind::Equal) {
            let value = self.parse_expr()?;
            Ok(Expr::AssignIndex {
                array: Box::new(array),
                index: Box::new(index),
                value: Box::new(value),
            })
        } else {
            Ok(Expr::Index {
                array: Box::new(array),
                index: Box::new(index),
            })
        }
    }

    fn parse_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::new();

        if !self.match_token(&TokenKind::RightParen) {
            loop {
                let arg = self.parse_expr()?;
                args.push(arg);
                if !self.match_token(&TokenKind::Comma) {
                    self.consume_token(TokenKind::RightParen, "Expected ')' after arguments")?;
                    break;
                }
            }
        }

        Ok(Expr::Call {
            callee: Box::new(callee),
            args,
        })
    }

    fn parse_binary(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let op = self.previous.kind.clone();
        let precedence = self.get_rule(&op).precedence;
        let right = self.parse_precedence(precedence.next())?;
        Ok(Expr::Binary {
            left: Box::new(left),
            op: Self::map_binary_op(self, op)?,
            right: Box::new(right),
        })
    }

    fn parse_arithmetic_assignment(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let op = self.previous.kind.clone();
        let value = self.parse_precedence(Precedence::Assignment)?;

        match left {
            Expr::Identifier(name) => {
                let op = match op {
                    TokenKind::PlusEqual => BinaryOp::Plus,
                    TokenKind::MinusEqual => BinaryOp::Minus,
                    TokenKind::StarEqual => BinaryOp::Star,
                    TokenKind::SlashEqual => BinaryOp::Slash,
                    TokenKind::PercentEqual => BinaryOp::Percent,
                    _ => unreachable!(),
                };

                Ok(Expr::AssignOp {
                    name,
                    op,
                    value: Box::new(value),
                })
            }
            _ => Err(ParseError::new(
                "Invalid assignment target",
                &self.filename,
                &self.previous,
            )),
        }
    }

    fn parse_number(&mut self) -> Result<Expr, ParseError> {
        if let TokenKind::Number(n) = self.previous.kind {
            Ok(Expr::Number(n))
        } else {
            Err(ParseError::new(
                "Expected number",
                &self.filename,
                self.peek(),
            ))
        }
    }

    fn parse_identifier(&mut self) -> Result<Expr, ParseError> {
        if let TokenKind::Identifier(name) = self.previous.kind.clone() {
            Ok(Expr::Identifier(name))
        } else {
            Err(ParseError::new(
                "Expected identifier",
                &self.filename,
                &self.previous,
            ))
        }
    }

    fn parse_assignment(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let value = self.parse_precedence(Precedence::Assignment)?;

        match left {
            Expr::Identifier(name) => Ok(Expr::Assign {
                name,
                value: Box::new(value),
            }),
            _ => Err(ParseError::new(
                "Invalid assignment target",
                &self.filename,
                &self.previous,
            )),
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        self.consume_token(TokenKind::LeftParen, "Expected '(' after 'fn'")?;

        let mut params = Vec::new();
        if self.peek().kind != TokenKind::RightParen {
            loop {
                self.advance();
                let name_expr = self.parse_identifier()?;
                let name = match name_expr {
                    Expr::Identifier(name) => name,
                    _ => unreachable!(),
                };

                self.consume_token(TokenKind::Colon, "Expected ':' after parameter name")?;
                let ty = self.parse_type()?;

                params.push((name, ty));

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let arity = params.len();
        if arity > constants::MAX_ARITY {
            return Err(ParseError::new(
                "Lambda arity exceeds maximum limit",
                &self.filename,
                self.peek(),
            ));
        }

        self.consume_token(TokenKind::RightParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&TokenKind::Arrow) {
            self.parse_type()?
        } else {
            Type::Umbra
        };

        self.consume_token(TokenKind::LeftBrace, "Expected '{' before lambda body")?;

        let body = self.parse_block()?;

        Ok(Expr::Lambda {
            arity,
            params,
            body,
            return_type,
        })
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !self.match_token(&TokenKind::RightBrace) {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_for(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume for

        self.consume_token(TokenKind::LeftParen, "Expected '(' after 'for'")?;

        let init = if self.peek().kind != TokenKind::Semicolon {
            Some(Box::new(self.parse_statement()?))
        } else {
            self.advance(); // skip ';'
            None
        };

        let condition = if self.peek().kind != TokenKind::Semicolon {
            let cond = self.parse_expr()?;
            self.consume_token(
                TokenKind::Semicolon,
                "Expected ';' after condition in for loop",
            )?;
            Some(cond)
        } else {
            self.advance();
            None
        };

        let post = if self.peek().kind != TokenKind::RightParen {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.consume_token(TokenKind::RightParen, "Expected ')' after for clause")?;
        self.consume_token(TokenKind::LeftBrace, "Expected '{' after for clause")?;

        let body = self.parse_block()?;

        Ok(Stmt::For {
            init,
            condition,
            post,
            body,
        })
    }

    fn parse_switch(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'switch'

        let target = self.parse_expr()?;
        self.consume_token(TokenKind::LeftBrace, "Expected '{' after switch expression")?;

        let mut cases = Vec::new();
        let mut default = None;

        while !self.match_token(&TokenKind::RightBrace) {
            if self.match_token(&TokenKind::Case) {
                let value = self.parse_expr()?;
                self.consume_token(TokenKind::LeftBrace, "Expected '{' after case value")?;
                let body = self.parse_block()?;
                cases.push((value, body));
            } else if self.match_token(&TokenKind::Default) {
                self.consume_token(TokenKind::LeftBrace, "Expected '{' after default")?;
                default = Some(self.parse_block()?);
            } else {
                return Err(ParseError::new(
                    "Expected 'case' or 'default'",
                    &self.filename,
                    self.peek(),
                ));
            }
        }

        Ok(Stmt::Switch {
            target,
            cases,
            default,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume if

        let condition = self.parse_expr()?;
        self.consume_token(TokenKind::LeftBrace, "Expected '{' after if condition")?;

        let body = self.parse_block()?;

        let else_body = if self.match_token(&TokenKind::Else) {
            if self.peek().kind == TokenKind::If {
                let stmt = self.parse_if()?;
                Some(vec![stmt])
            } else {
                self.consume_token(TokenKind::LeftBrace, "Expected '{' after else")?;
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            body,
            else_body,
        })
    }

    fn parse_fn(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume fn

        let name = match &self.advance().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "Expected function name",
                    &self.filename,
                    self.peek(),
                ));
            }
        };

        self.consume_token(TokenKind::LeftParen, "Expected '(' after function name")?;

        let mut params = Vec::new();
        if !self.match_token(&TokenKind::RightParen) {
            loop {
                let ident = match &self.advance().kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => {
                        return Err(ParseError::new(
                            "Expected parameter name",
                            &self.filename,
                            self.peek(),
                        ));
                    }
                };

                self.consume_token(TokenKind::Colon, "Expected ':' after parameter name")?;

                let ty = self.parse_type()?;

                params.push((ident, ty));

                if !self.match_token(&TokenKind::Comma) {
                    self.consume_token(TokenKind::RightParen, "Expected ')' after parameters")?;
                    break;
                }
            }
        }

        let arity = params.len();
        if arity > constants::MAX_ARITY {
            return Err(ParseError::new(
                "Function arity exceeds maximum limit",
                &self.filename,
                self.peek(),
            ));
        }

        let return_type = if self.match_token(&TokenKind::Arrow) {
            self.parse_type()?
        } else {
            Type::Umbra
        };

        self.consume_token(TokenKind::LeftBrace, "Expect '{' before function body.")?;

        let body = self.parse_block()?; // returns Vec<Stmt>

        Ok(Stmt::FnDecl {
            name,
            params,
            arity,
            body,
            return_type,
        })
    }

    fn parse_const(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume const
        let name = match &self.peek().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "expected identifier in const declaration",
                    &self.filename,
                    self.peek(),
                ));
            }
        };

        self.advance(); // consume identifier
        self.consume_token(
            TokenKind::Colon,
            "expected ':' after identifier in const declaration",
        )?;

        let ty = self.parse_type()?;

        self.consume_token(
            TokenKind::Equal,
            "expected '=' in const declaration — variable must be initialized",
        )?;

        let value = self.parse_binary_expr()?;

        self.consume_token(TokenKind::Semicolon, "expected ';' after const declaration")?;

        Ok(Stmt::ConstDecl { name, ty, value })
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume let

        let name = match &self.peek().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "expected identifier in let declaration",
                    &self.filename,
                    self.peek(),
                ));
            }
        };

        self.advance(); // consume identifier
        self.consume_token(
            TokenKind::Colon,
            "expected ':' after identifier in let declaration",
        )?;

        let ty = self.parse_type()?;

        self.consume_token(
            TokenKind::Equal,
            "expected '=' in let declaration — variable must be initialized",
        )?;

        let value = self.parse_binary_expr()?;

        if let Expr::Lambda { .. } = value {
            self.match_token(&TokenKind::Semicolon);
        } else {
            self.consume_token(TokenKind::Semicolon, "expected ';' after let declaration")?;
        }

        Ok(Stmt::LetDecl { name, ty, value })
    }

    fn parse_literal(&mut self) -> Result<Expr, ParseError> {
        match &self.previous.kind {
            TokenKind::True => Ok(Expr::Bool(true)),
            TokenKind::False => Ok(Expr::Bool(false)),
            TokenKind::Umbra => Ok(Expr::Umbra),
            _ => Err(ParseError::new(
                "Expected literal",
                &self.filename,
                self.peek(),
            )),
        }
    }

    fn parse_string(&mut self) -> Result<Expr, ParseError> {
        if let TokenKind::String(ref s) = self.previous.kind {
            Ok(Expr::String(s.clone()))
        } else {
            Err(ParseError::new(
                "Expected string",
                &self.filename,
                self.peek(),
            ))
        }
    }

    fn parse_precedence(&mut self, min_prec: Precedence) -> Result<Expr, ParseError> {
        let token = self.advance().clone();
        let prefix_rule = self
            .get_rule(&token.kind)
            .prefix
            .ok_or_else(|| ParseError::new("Expected expression", &self.filename, self.peek()))?;

        let mut left = prefix_rule(self)?;

        loop {
            let rule = self.get_rule(&self.peek().kind);
            if rule.precedence < min_prec {
                break;
            }

            let infix = match rule.infix {
                Some(func) => func,
                None => break,
            };

            self.advance();
            left = infix(self, left)?;
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let op = self.previous.kind.clone();
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expr::Unary {
            op: Self::map_unary_op(self, op)?,
            expr: Box::new(right),
        })
    }

    fn parse_logical(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let op = match self.previous.kind {
            TokenKind::AndAnd => LogicalOp::AndAnd,
            TokenKind::OrOr => LogicalOp::OrOr,
            _ => {
                return Err(ParseError::new(
                    format!("Invalid logical operator: {:?}", self.previous.kind),
                    &self.filename,
                    self.peek(),
                ));
            }
        };

        let right = self.parse_precedence(Precedence::And)?;
        Ok(Expr::Logical {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek().kind {
            TokenKind::Constellation => self.parse_constellation(),
            TokenKind::Const => self.parse_const(),
            TokenKind::Let => self.parse_let(),
            TokenKind::Fn => self.parse_fn(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::Switch => self.parse_switch(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Emit => self.parse_emit(),
            TokenKind::Return => self.parse_return(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_constellation(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume constellation
        let name = match &self.peek().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "Expected identifier",
                    &self.filename,
                    self.peek(),
                ));
            }
        };

        self.advance(); // consume identifier

        self.consume_token(
            TokenKind::Semicolon,
            "Expected ';' after constellation declaration",
        )?;

        Ok(Stmt::ConstellationDecl(name))
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume return
        let value = if !self.match_token(&TokenKind::Semicolon) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.consume_token(TokenKind::Semicolon, "Expected ';' after return")?;

        Ok(Stmt::Return(value))
    }

    fn parse_break(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'break'
        self.consume_token(TokenKind::Semicolon, "Expected ';' after break")?;
        Ok(Stmt::Break)
    }

    fn parse_continue(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'continue'
        self.consume_token(TokenKind::Semicolon, "Expected ';' after continue")?;
        Ok(Stmt::Continue)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let token = self.peek().clone();
        self.advance();
        match &token.kind {
            TokenKind::Light => Ok(Type::Light),
            TokenKind::Lumens => Ok(Type::Lumens),
            TokenKind::Umbra => Ok(Type::Umbra),
            TokenKind::Photon => Ok(Type::Photon),
            TokenKind::Lambda => Ok(Type::Lambda),
            TokenKind::LeftBracket => self.parse_array_type(),
            TokenKind::Identifier(name) => Ok(Type::Custom(name.clone())),
            _ => Err(ParseError::new(
                format!("Invalid type: {:?}", token.kind),
                &self.filename,
                self.peek(),
            )),
        }
    }

    fn parse_array_type(&mut self) -> Result<Type, ParseError> {
        let element_type = self.parse_type()?;
        self.consume_token(TokenKind::RightBracket, "Expected ']' after array type")?;
        Ok(Type::Array(Box::new(element_type)))
    }

    fn peek(&self) -> &Token {
        &self.current
    }

    fn get_rule(&self, kind: &TokenKind) -> ParseRule<'a> {
        use TokenKind::*;
        match kind {
            OrOr => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_logical),
                precedence: Precedence::Or,
            },
            AndAnd => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_logical),
                precedence: Precedence::And,
            },
            Bang => ParseRule {
                prefix: Some(Parser::parse_unary),
                infix: None,
                precedence: Precedence::None,
            },
            EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Equality,
            },
            BangEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Equality,
            },
            Greater => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Comparison,
            },
            GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Comparison,
            },
            Less => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Comparison,
            },
            LessEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Comparison,
            },
            Number(_) => ParseRule {
                prefix: Some(Parser::parse_number),
                infix: None,
                precedence: Precedence::None,
            },
            Plus => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Term,
            },
            Minus => ParseRule {
                prefix: Some(Parser::parse_unary),
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Term,
            },
            Star | Slash | Percent => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Factor,
            },
            PlusEqual | MinusEqual | StarEqual | SlashEqual | PercentEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_arithmetic_assignment),
                precedence: Precedence::Assignment,
            },
            LeftParen => ParseRule {
                prefix: Some(Parser::parse_grouping),
                infix: Some(Parser::parse_call),
                precedence: Precedence::Call,
            },
            LeftBracket => ParseRule {
                prefix: Some(Parser::parse_array_literal),
                infix: Some(Parser::parse_index),
                precedence: Precedence::Call,
            },
            True | False | Umbra => ParseRule {
                prefix: Some(Parser::parse_literal),
                infix: None,
                precedence: Precedence::None,
            },
            String(_) => ParseRule {
                prefix: Some(Parser::parse_string),
                infix: None,
                precedence: Precedence::None,
            },
            Identifier(_) => ParseRule {
                prefix: Some(Parser::parse_identifier),
                infix: None,
                precedence: Precedence::None,
            },
            Equal => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_assignment),
                precedence: Precedence::Assignment,
            },
            Fn => ParseRule {
                prefix: Some(Parser::parse_lambda),
                infix: None,
                precedence: Precedence::None,
            },
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}
