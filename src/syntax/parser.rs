use crate::error::ParseError;
use crate::tir::ExprId;
use crate::types::LuxType;
use crate::{constants, get_source_line};

use super::ast::{BinaryOp, Expr, LogicalOp, MethodSig, Module, Span, Spanned, Stmt, UnaryOp};
use super::lexer::Lexer;
use super::precedence::Precedence;
use super::token::{Token, TokenKind};

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Result<Spanned<Expr>, ParseError>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Spanned<Expr>) -> Result<Spanned<Expr>, ParseError>;

struct ParseRule<'a> {
    prefix: Option<PrefixParseFn<'a>>,
    infix: Option<InfixParseFn<'a>>,
    precedence: Precedence,
}

pub struct Parser<'a> {
    next_expr_id: usize,
    filename: String,
    source: &'a String,
    previous: Token,
    current: Token,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(filename: &str, source: &'a String, mut lexer: Lexer<'a>) -> Self {
        let first = lexer.next_token();

        Parser {
            next_expr_id: 0,
            filename: filename.to_string(),
            source,
            lexer,
            previous: first.clone(),
            current: first.clone(),
        }
    }

    fn fresh_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let mut body = Vec::new();

        while !self.is_at_end() {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        Ok(Module::new(body))
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
            Err(ParseError::new(
                message,
                &self.filename,
                self.peek(),
                &get_source_line(&self.source, self.peek().line),
            ))
        }
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
            TokenKind::StarStar => Ok(BinaryOp::StarStar),
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
                    &get_source_line(&self.source, self.peek().line),
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
                &get_source_line(&self.source, self.peek().line),
            )),
        }
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn parse_emit(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume emit

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::Semicolon, "Expected ';' after emit")?;
        Ok(Spanned::new(self.fresh_expr_id(), Stmt::Emit(expr), span))
    }

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_expr_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let expr = self.parse_expr()?;
        let span = expr.span.clone();
        self.consume_token(TokenKind::Semicolon, "Expected ';' after expression")?;
        Ok(Spanned::new(self.fresh_expr_id(), Stmt::Expr(expr), span))
    }

    fn parse_grouping(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::RightParen, "Expected ')' after expression")?;
        Ok(expr)
    }

    fn parse_array_literal(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut elements = Vec::new();

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

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

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Array { elements },
            span,
        ))
    }

    fn parse_index(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let index = self.parse_expr()?;
        let span_start = index.span.clone();
        self.consume_token(TokenKind::RightBracket, "Expected ']' after index")?;

        if let Expr::Range { .. } = index.node {
            let span_start = index.span.clone();
            return Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::Slice {
                    callee: Box::new(left),
                    range: Box::new(index),
                },
                span_start,
            ));
        }

        if self.match_token(&TokenKind::Equal) {
            let value = self.parse_expr()?;
            Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::AssignIndex {
                    callee: Box::new(left),
                    index: Box::new(index),
                    value: Box::new(value),
                },
                span_start,
            ))
        } else {
            Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::Index {
                    callee: Box::new(left),
                    index: Box::new(index),
                },
                span_start,
            ))
        }
    }

    fn parse_method_call(&mut self, receiver: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let name = match &self.peek().kind {
            TokenKind::Identifier(n) => n.clone(),
            _ => {
                return Err(ParseError::new(
                    "Expected method name after '::'",
                    &self.filename,
                    self.peek(),
                    &get_source_line(&self.source, self.peek().line),
                ));
            }
        };

        let span = Span {
            filename: self.filename.clone(),
            line: self.peek().line,
            column: self.peek().column,
            column_start: self.peek().column_start,
        };

        self.advance(); // consume identifier

        self.consume_token(TokenKind::LeftParen, "Expected '(' after method name")?;
        let mut args = Vec::new();
        if !self.match_token(&TokenKind::RightParen) {
            loop {
                args.push(self.parse_expr()?);
                if !self.match_token(&TokenKind::Comma) {
                    self.consume_token(TokenKind::RightParen, "Expected ')' after arguments")?;
                    break;
                }
            }
        }

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::MethodCall {
                receiver: Box::new(receiver),
                method: name,
                args,
            },
            span,
        ))
    }

    fn parse_call(&mut self, callee: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
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

        let span_start = callee.span.clone();
        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Call {
                callee: Box::new(callee),
                args,
            },
            span_start,
        ))
    }

    fn parse_field_access(&mut self, object: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let field = self.consume_identifier("Expected field name after '.'")?;

        let span_start = object.span.clone();
        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::FieldGet {
                object: Box::new(object),
                field,
            },
            span_start,
        ))
    }

    fn parse_binary(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let op = self.previous.kind.clone();
        let precedence = self.get_rule(&op).precedence;
        let right = self.parse_precedence(precedence.next())?;
        let span_start = left.span.clone();
        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Binary {
                left: Box::new(left),
                op: Self::map_binary_op(self, op)?,
                right: Box::new(right),
            },
            span_start,
        ))
    }

    fn parse_range(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let op = self.previous.kind.clone();
        let precedence = self.get_rule(&op).precedence;
        let right = self.parse_precedence(precedence.next())?;
        let span_start = left.span.clone();
        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Range {
                start: Box::new(left),
                end: Box::new(right),
            },
            span_start,
        ))
    }

    fn parse_arithmetic_assignment(
        &mut self,
        left: Spanned<Expr>,
    ) -> Result<Spanned<Expr>, ParseError> {
        let op = self.previous.kind.clone();
        let value = self.parse_precedence(Precedence::Assignment)?;
        let span_start = left.span.clone();

        match left.node {
            Expr::Identifier(name) => {
                let op = match op {
                    TokenKind::PlusEqual => BinaryOp::Plus,
                    TokenKind::MinusEqual => BinaryOp::Minus,
                    TokenKind::StarEqual => BinaryOp::Star,
                    TokenKind::SlashEqual => BinaryOp::Slash,
                    TokenKind::PercentEqual => BinaryOp::Percent,
                    TokenKind::StarStarEqual => BinaryOp::StarStar,
                    _ => unreachable!(),
                };

                Ok(Spanned::new(
                    self.fresh_expr_id(),
                    Expr::AssignOp {
                        name,
                        op,
                        value: Box::new(value),
                    },
                    span_start,
                ))
            }
            _ => Err(ParseError::new(
                "Invalid assignment target",
                &self.filename,
                &self.previous,
                &get_source_line(&self.source, self.previous.line),
            )),
        }
    }

    fn parse_number(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        if let TokenKind::Number(n) = self.previous.kind {
            Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::Number(n),
                span_start,
            ))
        } else {
            Err(ParseError::new(
                "Expected number",
                &self.filename,
                self.peek(),
                &get_source_line(&self.source, self.peek().line),
            ))
        }
    }

    fn parse_identifier(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let token = self.previous.clone();
        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        if let TokenKind::Identifier(name) = token.kind {
            if self.match_token(&TokenKind::LeftBrace) {
                match name.as_str() {
                    "Map" => Ok(self.parse_map_literal()?),
                    _ => Ok(self.parse_facet_literal(name)?),
                }
            } else {
                Ok(Spanned::new(
                    self.fresh_expr_id(),
                    Expr::Identifier(name),
                    span_start,
                ))
            }
        } else {
            Err(ParseError::new(
                "Expected identifier",
                &self.filename,
                &self.previous,
                &get_source_line(&self.source, self.previous.line),
            ))
        }
    }

    fn parse_assignment(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let value = self.parse_precedence(Precedence::Assignment)?;
        let span_start = left.span.clone();

        match left.node {
            Expr::Identifier(name) => Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::Assign {
                    name,
                    value: Box::new(value),
                },
                span_start,
            )),
            _ => Err(ParseError::new(
                "Invalid assignment target",
                &self.filename,
                &self.previous,
                &get_source_line(&self.source, self.previous.line),
            )),
        }
    }

    fn parse_lambda(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.consume_token(TokenKind::LeftParen, "Expected '(' after 'fn'")?;

        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let mut params = Vec::new();
        if self.peek().kind != TokenKind::RightParen {
            loop {
                self.advance();
                let name_expr = self.parse_identifier()?;
                let name = match name_expr.node {
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
                &get_source_line(&self.source, self.peek().line),
            ));
        }

        self.consume_token(TokenKind::RightParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&TokenKind::Arrow) {
            self.parse_type()?
        } else {
            LuxType::Umbra
        };

        self.consume_token(TokenKind::LeftBrace, "Expected '{' before lambda body")?;

        let body = self.parse_block()?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Lambda {
                arity,
                params,
                body,
                return_type,
            },
            span_start,
        ))
    }

    fn parse_block(&mut self) -> Result<Vec<Spanned<Stmt>>, ParseError> {
        let mut stmts = Vec::new();
        while !self.match_token(&TokenKind::RightBrace) {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_for(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume for

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

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

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::For {
                init,
                condition,
                post,
                body,
            },
            span,
        ))
    }

    fn parse_switch(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume 'switch'

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        self.consume_token(TokenKind::LeftParen, "Expected '(' after 'switch'")?;

        let target = self.parse_expr()?;

        self.consume_token(
            TokenKind::RightParen,
            "Expected ')' after switch expression",
        )?;
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
                    &get_source_line(&self.source, self.peek().line),
                ));
            }
        }

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::Switch {
                target,
                cases,
                default,
            },
            span,
        ))
    }

    fn parse_if(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume if

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        self.consume_token(TokenKind::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expr()?;
        self.consume_token(TokenKind::RightParen, "Expected ')' after if condition")?;
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

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::If {
                condition,
                body,
                else_body,
            },
            span,
        ))
    }

    fn parse_fn(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume fn

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let name = match &self.advance().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "Expected function name",
                    &self.filename,
                    self.peek(),
                    &get_source_line(&self.source, self.peek().line),
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
                            &get_source_line(&self.source, self.peek().line),
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
                &get_source_line(&self.source, self.peek().line),
            ));
        }

        let return_type = if self.match_token(&TokenKind::Arrow) {
            self.parse_type()?
        } else {
            LuxType::Umbra
        };

        self.consume_token(TokenKind::LeftBrace, "Expect '{' before function body.")?;

        let body = self.parse_block()?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::FnDecl {
                name,
                params,
                arity,
                body,
                return_type,
            },
            span,
        ))
    }

    fn parse_const(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume const

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let name = self.consume_identifier("expected identifier in const declaration")?;

        self.consume_token(
            TokenKind::Colon,
            "expected ':' after identifier in const declaration",
        )?;

        let ty = self.parse_type()?;

        self.consume_token(
            TokenKind::Equal,
            "expected '=' in const declaration — variable must be initialized",
        )?;

        let value = self.parse_expr()?;

        self.consume_token(TokenKind::Semicolon, "expected ';' after const declaration")?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::ConstDecl { name, ty, value },
            span,
        ))
    }

    fn parse_let(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume let

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let name = self.consume_identifier("expected identifier in let declaration")?;
        self.consume_token(
            TokenKind::Colon,
            "expected ':' after identifier in let declaration",
        )?;
        let ty = self.parse_type()?;

        let value = if self.match_token(&TokenKind::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let is_lambda_init = if let Some(val_expr) = value.as_ref() {
            matches!(val_expr.node, Expr::Lambda { .. })
        } else {
            false
        };

        if is_lambda_init {
            self.match_token(&TokenKind::Semicolon);
        } else {
            self.consume_token(TokenKind::Semicolon, "expected ';' after let declaration")?;
        }

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::LetDecl { name, ty, value },
            span,
        ))
    }

    fn parse_radiate_decl(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume ‘Radiate’

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let facet_name = match &self.advance().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "Expected facet name after 'Radiate'",
                    &self.filename,
                    self.peek(),
                    &get_source_line(&self.source, self.peek().line),
                ));
            }
        };

        self.consume_token(TokenKind::LeftBrace, "Expected '{' after facet name")?;

        let mut methods = Vec::new();

        while !self.match_token(&TokenKind::RightBrace) {
            methods.push(self.parse_method(&facet_name)?);
        }

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::RadiateDecl {
                facet_name,
                methods,
            },
            span,
        ))
    }

    /// Parses a method signature and body within a Facet definition.
    /// Handles optional `@static` annotation and implicit `self` parameter for instance methods.
    fn parse_method(&mut self, facet_name: &String) -> Result<MethodSig, ParseError> {
        let mut is_static = false;

        let token = self.peek();
        if let TokenKind::Identifier(ident) = &token.kind {
            if ident == "@static" {
                self.advance(); // Consume the "@static" token
                is_static = true;
            }
        }

        self.advance(); // consume method name

        let name = match &self.previous.kind {
            // Ensure the identifier is not "@static" itself (handles error case: '@static @static')
            TokenKind::Identifier(n) if n != "@static" => n.clone(),
            _ if is_static => {
                return Err(ParseError::new(
                    "Expected method name after @static annotation",
                    &self.filename,
                    &self.previous,
                    &get_source_line(&self.source, self.previous.line),
                ));
            }

            _ => {
                return Err(ParseError::new(
                    "Expected method name",
                    &self.filename,
                    &self.previous,
                    &get_source_line(&self.source, self.previous.line),
                ));
            }
        };

        self.consume_token(TokenKind::LeftParen, "Expected '(' after method name")?;

        let mut explicit_params: Vec<(String, LuxType)> = Vec::new();

        if !self.match_token(&TokenKind::RightParen) {
            loop {
                let param_name_token = self.peek().clone();
                let param_name = match &param_name_token.kind {
                    TokenKind::Identifier(n) => n.clone(),
                    _ => {
                        return Err(ParseError::new(
                            "Expected parameter name",
                            &self.filename,
                            &param_name_token,
                            &get_source_line(&self.source, param_name_token.line),
                        ));
                    }
                };

                self.advance();

                self.consume_token(TokenKind::Colon, "Expected ':' after parameter name")?;

                let param_type = self.parse_type()?;

                explicit_params.push((param_name, param_type));

                if self.match_token(&TokenKind::RightParen) {
                    break;
                } else {
                    self.consume_token(TokenKind::Comma, "Expected ',' or ')' after parameter")?;
                    if self.match_token(&TokenKind::RightParen) {
                        break;
                    }
                }
            }
        }

        let final_params = if !is_static {
            let mut final_list = vec![("self".to_string(), LuxType::Named(facet_name.clone()))];
            final_list.extend(explicit_params);
            final_list
        } else {
            explicit_params
        };

        let arity = final_params.len();

        let return_type = if self.match_token(&TokenKind::Arrow) {
            self.parse_type()?
        } else {
            LuxType::Umbra
        };

        self.consume_token(TokenKind::LeftBrace, "Expected '{' before method body")?;
        let body = self.parse_block()?;

        Ok(MethodSig {
            name,
            params: final_params,
            arity,
            body,
            return_type,
            is_static,
        })
    }

    fn parse_refraction_decl(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume 'Refraction'

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let name = self.consume_identifier("Expected type name after 'Refraction'")?;

        match self.peek().kind {
            TokenKind::Facet => self.parse_facet_body(name),
            _ => {
                // Assume type alias
                let alias_type = self.parse_type()?;

                Ok(Spanned::new(
                    self.fresh_expr_id(),
                    Stmt::TypeAlias {
                        name,
                        aliased: alias_type,
                    },
                    span,
                ))
            }
        }
    }

    fn parse_facet_body(&mut self, name: String) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume 'Facet'

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        self.consume_token(TokenKind::LeftBrace, "Expected '{' after 'Facet'")?;

        let mut fields = Vec::new();

        while self.peek().kind != TokenKind::RightBrace {
            let field_name = self.consume_identifier("Expected field name")?;
            let field_type = self.parse_type()?;
            fields.push((field_name, field_type));
        }

        self.consume_token(TokenKind::RightBrace, "Expected '}' after facet body")?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::FacetDecl { name, fields },
            span,
        ))
    }

    fn parse_map_literal(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut fields = Vec::new();

        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        while self.peek().kind != TokenKind::RightBrace {
            let key = self.parse_expr()?;
            self.consume_token(TokenKind::Colon, "Expected ':' after key")?;
            let value = self.parse_expr()?;
            fields.push((key, value));

            if self.peek().kind != TokenKind::RightBrace {
                self.consume_token(TokenKind::Comma, "Expected ',' after map entry")?;
            }
        }

        self.consume_token(TokenKind::RightBrace, "Expected '}' after facet literal")?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Map(fields),
            span_start,
        ))
    }

    fn parse_facet_literal(&mut self, type_name: String) -> Result<Spanned<Expr>, ParseError> {
        let mut fields = Vec::new();

        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        while self.peek().kind != TokenKind::RightBrace {
            let field_name = self.consume_identifier("Expected field name")?;
            self.consume_token(TokenKind::Colon, "Expected ':' after field name")?;
            let expr = self.parse_expr()?;
            fields.push((field_name, expr));

            self.match_token(&TokenKind::Comma); // optional comma
        }

        self.consume_token(TokenKind::RightBrace, "Expected '}' after facet literal")?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::FacetInit { type_name, fields },
            span_start,
        ))
    }

    fn parse_literal(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        match &self.previous.kind {
            TokenKind::True => Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::Boolean(true),
                span_start,
            )),
            TokenKind::False => Ok(Spanned::new(
                self.fresh_expr_id(),
                Expr::Boolean(false),
                span_start,
            )),
            TokenKind::Identifier(name) => match name.as_str() {
                "Umbra" => Ok(Spanned::new(self.fresh_expr_id(), Expr::Umbra, span_start)),
                _ => Err(ParseError::new(
                    "Expected literal",
                    &self.filename,
                    self.peek(),
                    &get_source_line(&self.source, self.peek().line),
                )),
            },
            _ => Err(ParseError::new(
                "Expected literal",
                &self.filename,
                self.peek(),
                &get_source_line(&self.source, self.peek().line),
            )),
        }
    }

    fn parse_string(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let span_start = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let string_literal = if let TokenKind::String(ref s) = self.previous.kind {
            s.clone()
        } else {
            return Err(ParseError::new(
                "Expected string",
                &self.filename,
                self.peek(),
                &get_source_line(&self.source, self.peek().line),
            ));
        };

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::String(string_literal),
            span_start,
        ))
    }

    fn parse_precedence(&mut self, min_prec: Precedence) -> Result<Spanned<Expr>, ParseError> {
        let token = self.advance().clone();
        let prefix_rule = self.get_rule(&token.kind).prefix.ok_or_else(|| {
            ParseError::new(
                "Expected expression",
                &self.filename,
                self.peek(),
                &get_source_line(&self.source, self.peek().line),
            )
        })?;

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

    fn parse_unary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let op = self.previous.kind.clone();
        let right = self.parse_precedence(Precedence::Unary)?;
        let span = right.span.clone();

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Unary {
                op: Self::map_unary_op(self, op)?,
                expr: Box::new(right),
            },
            span,
        ))
    }

    fn parse_logical(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let op = match self.previous.kind {
            TokenKind::AndAnd => LogicalOp::AndAnd,
            TokenKind::OrOr => LogicalOp::OrOr,
            _ => {
                return Err(ParseError::new(
                    format!("Invalid logical operator: {:?}", self.previous.kind),
                    &self.filename,
                    self.peek(),
                    &get_source_line(&self.source, self.peek().line),
                ));
            }
        };

        let right = self.parse_precedence(Precedence::And)?;
        let span_start = left.span.clone();
        Ok(Spanned::new(
            self.fresh_expr_id(),
            Expr::Logical {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span_start,
        ))
    }

    fn parse_statement(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        match self.peek().kind {
            TokenKind::Constellation => self.parse_constellation(),
            TokenKind::Const => self.parse_const(),
            TokenKind::Let => self.parse_let(),
            TokenKind::Refraction => self.parse_refraction_decl(),
            TokenKind::Radiate => self.parse_radiate_decl(),
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

    fn parse_constellation(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume constellation

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let name = match &self.peek().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "Expected identifier",
                    &self.filename,
                    self.peek(),
                    &get_source_line(&self.source, self.peek().line),
                ));
            }
        };

        self.advance(); // consume identifier

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::ConstellationDecl(name),
            span,
        ))
    }

    fn parse_return(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume return

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        let value = if !self.match_token(&TokenKind::Semicolon) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.consume_token(TokenKind::Semicolon, "Expected ';' after return")?;

        Ok(Spanned::new(
            self.fresh_expr_id(),
            Stmt::Return(value),
            span,
        ))
    }

    fn parse_break(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume 'break'

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        self.consume_token(TokenKind::Semicolon, "Expected ';' after break")?;
        Ok(Spanned::new(self.fresh_expr_id(), Stmt::Break, span))
    }

    fn parse_continue(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume 'continue'

        let span = Span {
            filename: self.filename.clone(),
            line: self.previous.line,
            column: self.previous.column,
            column_start: self.previous.column_start,
        };

        self.consume_token(TokenKind::Semicolon, "Expected ';' after continue")?;
        Ok(Spanned::new(self.fresh_expr_id(), Stmt::Continue, span))
    }

    fn parse_type(&mut self) -> Result<LuxType, ParseError> {
        let left = self.parse_type_primary()?;

        Ok(left)
    }

    fn parse_type_primary(&mut self) -> Result<LuxType, ParseError> {
        let token = self.advance();

        match &token.kind {
            TokenKind::LeftParen => {
                let mut params = vec![];

                if self.peek().kind != TokenKind::RightParen {
                    loop {
                        params.push(self.parse_type()?);
                        if self.peek().kind != TokenKind::Comma {
                            break;
                        }
                        self.advance(); // consume ','
                    }
                }

                self.consume_token(TokenKind::RightParen, "Expected ')' in function type")?;

                let next = self.peek();
                if next.kind == TokenKind::Arrow {
                    // We are parsing a function type
                    self.advance(); // consume '->'
                    let ret_type = self.parse_type()?;
                    Ok(LuxType::Function(params, Box::new(ret_type)))
                } else if params.len() == 1 {
                    Ok(params.into_iter().next().unwrap())
                } else {
                    return Err(ParseError::new(
                        "Unexpected multiple types in parentheses — expected function type",
                        &self.filename,
                        &next,
                        &get_source_line(&self.source, next.line),
                    ));
                }
            }

            TokenKind::Identifier(name) => match name.as_str() {
                "Light" => Ok(LuxType::Light),
                "Lumens" => Ok(LuxType::Lumens),
                "Photon" => Ok(LuxType::Photon),
                "Umbra" => Ok(LuxType::Umbra),
                "Range" => Ok(LuxType::Range),
                "Map" => self.parse_map_type(),
                "Function" => self.parse_function_type(),
                _ => Ok(LuxType::Named(name.clone())), // Handle custom type names
            },
            TokenKind::LeftBracket => self.parse_array_type(),
            _ => Err(ParseError::new(
                format!("Invalid type: {:?}", token.kind),
                &self.filename,
                self.peek(),
                &get_source_line(&self.source, self.peek().line),
            )),
        }
    }

    fn parse_map_type(&mut self) -> Result<LuxType, ParseError> {
        self.consume_token(TokenKind::Less, "Expected '<' after Map")?;
        let key_type = self.parse_type()?;
        self.consume_token(TokenKind::Comma, "Expected ',' after Map key")?;
        let value_type = self.parse_type()?;
        self.consume_token(TokenKind::Greater, "Expected '>' after Map")?;
        Ok(LuxType::Map(Box::new(key_type), Box::new(value_type)))
    }

    fn parse_function_type(&mut self) -> Result<LuxType, ParseError> {
        self.consume_token(TokenKind::LeftParen, "Expected '(' after Function")?;
        self.consume_token(
            TokenKind::LeftBracket,
            "Expected '[' in Function parameters",
        )?;
        let mut params = Vec::new();
        while self.peek().kind != TokenKind::RightBracket {
            params.push(self.parse_type()?);
        }
        self.consume_token(TokenKind::RightBracket, "Expected ']' after parameters")?;
        self.consume_token(TokenKind::Comma, "Expected ',' after parameters")?;
        let return_type = self.parse_type()?;
        self.consume_token(TokenKind::RightParen, "Expected ')' after Function")?;
        Ok(LuxType::Function(params, Box::new(return_type)))
    }

    fn parse_array_type(&mut self) -> Result<LuxType, ParseError> {
        let element_type = self.parse_type()?;
        self.consume_token(TokenKind::RightBracket, "Expected ']' after array type")?;
        Ok(LuxType::Array(Box::new(element_type)))
    }

    fn peek(&self) -> &Token {
        &self.current
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        let token = self.advance().clone();

        if let TokenKind::Identifier(name) = &token.kind {
            Ok(name.clone())
        } else {
            Err(ParseError::new(
                message,
                &self.filename,
                &token,
                &get_source_line(&self.source, token.line),
            ))
        }
    }

    fn get_rule(&self, kind: &TokenKind) -> ParseRule<'a> {
        use super::token::TokenKind::*;
        match kind {
            DotDot => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_range),
                precedence: Precedence::Range,
            },
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
            EqualEqual | BangEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Equality,
            },
            Greater | Less | GreaterEqual | LessEqual => ParseRule {
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
            StarStar => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_binary),
                precedence: Precedence::Power,
            },
            PlusEqual | MinusEqual | StarEqual | SlashEqual | PercentEqual | StarStarEqual => {
                ParseRule {
                    prefix: None,
                    infix: Some(Parser::parse_arithmetic_assignment),
                    precedence: Precedence::Assignment,
                }
            }
            ColonColon => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_method_call),
                precedence: Precedence::Call,
            },
            LeftParen => ParseRule {
                prefix: Some(Parser::parse_grouping), // Grouping: (expr)
                infix: Some(Parser::parse_call),      // Function call: func(args)
                precedence: Precedence::Call,
            },
            LeftBracket => ParseRule {
                prefix: Some(Parser::parse_array_literal), // Array literal: [1, 2]
                infix: Some(Parser::parse_index), // Indexing: arr[idx], map[key], string[idx]
                precedence: Precedence::Call,
            },
            True | False => ParseRule {
                prefix: Some(Parser::parse_literal),
                infix: None,
                precedence: Precedence::None,
            },
            String(_) => ParseRule {
                prefix: Some(Parser::parse_string),
                infix: None,
                precedence: Precedence::None,
            },
            Identifier(name) => match name.as_str() {
                "Umbra" => ParseRule {
                    prefix: Some(Parser::parse_literal),
                    infix: None,
                    precedence: Precedence::None,
                },
                _ => ParseRule {
                    prefix: Some(Parser::parse_identifier),
                    infix: None,
                    precedence: Precedence::None,
                },
            },
            Equal => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_assignment),
                precedence: Precedence::Assignment,
            },
            Dot => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_field_access), // Field access: obj.field
                precedence: Precedence::Call,
            },
            Fn => ParseRule {
                prefix: Some(Parser::parse_lambda), // Lambda/Anonymous function: fn() -> T { ... }
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
