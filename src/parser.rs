use crate::ast::{BinaryOp, Expr, Stmt, Type, UnaryOp};
use crate::error::ParseError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! -
    Call,       // function calls
}

impl Precedence {
    fn next(self) -> Precedence {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Call, // or Highest
        }
    }
}

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Result<Expr, ParseError>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Expr) -> Result<Expr, ParseError>;

struct ParseRule<'a> {
    prefix: Option<PrefixParseFn<'a>>,
    infix: Option<InfixParseFn<'a>>,
    precedence: Precedence,
}

pub struct Parser<'a> {
    previous: Token,
    current: Token,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let first = lexer.next_token();

        Parser {
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
            Err(ParseError::new(message, self.peek()))
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

    fn map_binary_op(op: TokenKind) -> BinaryOp {
        match op {
            TokenKind::Plus => BinaryOp::Plus,
            TokenKind::Minus => BinaryOp::Minus,
            TokenKind::Star => BinaryOp::Star,
            TokenKind::Slash => BinaryOp::Slash,
            TokenKind::Percent => BinaryOp::Percent,
            TokenKind::EqualEqual => BinaryOp::EqualEqual,
            TokenKind::BangEqual => BinaryOp::BangEqual,
            TokenKind::Less => BinaryOp::Less,
            TokenKind::LessEqual => BinaryOp::LessEqual,
            TokenKind::Greater => BinaryOp::Greater,
            TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
            _ => panic!("Invalid binary operator: {:?}", op),
        }
    }

    fn map_unary_op(op: TokenKind) -> UnaryOp {
        match op {
            TokenKind::Bang => UnaryOp::Bang,
            TokenKind::Minus => UnaryOp::Minus,
            _ => panic!("Invalid unary operator: {:?}", op),
        }
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::EOF)
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

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::Semicolon, "Expected ';' after expression")?;
        Ok(Stmt::ExprStmt(expr))
    }

    fn parse_grouping(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr()?;
        self.consume_token(TokenKind::RightParen, "Expected ')' after expression")?;
        Ok(expr)
    }

    fn parse_infix_binary(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let op = self.previous.kind.clone();
        let precedence = self.get_rule(&op).precedence;
        let right = self.parse_precedence(precedence.next())?;
        Ok(Expr::Binary {
            left: Box::new(left),
            op: Self::map_binary_op(op),
            right: Box::new(right),
        })
    }

    fn parse_number(&mut self) -> Result<Expr, ParseError> {
        if let TokenKind::Number(n) = self.previous.kind {
            Ok(Expr::Number(n))
        } else {
            Err(ParseError::new("Expected number", &self.previous))
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.advance();

        let name = match &tok.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    "expected identifier in let declaration",
                    &tok,
                ));
            }
        };

        self.advance(); // consume identifier
        self.consume_token(
            TokenKind::Colon,
            "expected ':' after identifier in let declaration",
        )?;

        let ty = self
            .parse_type()
            .ok_or_else(|| ParseError::new("Expected type after ':'", self.peek()))?;

        self.consume_token(
            TokenKind::Equal,
            "expected '=' in let declaration — variable must be initialized",
        )?;

        let value = self.parse_binary_expr()?;

        self.consume_token(TokenKind::Semicolon, "expected ';' after let declaration")?;

        Ok(Stmt::LetDecl { name, ty, value })
    }

    fn parse_literal(&mut self) -> Result<Expr, ParseError> {
        match &self.previous.kind {
            TokenKind::True => Ok(Expr::Bool(true)),
            TokenKind::False => Ok(Expr::Bool(false)),
            _ => Err(ParseError::new("Expected literal", &self.previous)),
        }
    }

    fn parse_precedence(&mut self, min_prec: Precedence) -> Result<Expr, ParseError> {
        let token = self.advance().clone();

        let prefix_rule = self
            .get_rule(&token.kind)
            .prefix
            .ok_or_else(|| ParseError::new("Expected expression", &token))?;

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

    fn parse_prefix_unary(&mut self) -> Result<Expr, ParseError> {
        let op = self.previous.kind.clone();
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expr::Unary {
            op: Self::map_unary_op(op),
            expr: Box::new(right),
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek().kind {
            TokenKind::Let => self.parse_let(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let token = self.peek().clone();
        self.advance();
        match &token.kind {
            TokenKind::Light => Some(Type::Light),
            TokenKind::Lumens => Some(Type::Lumens),
            TokenKind::Umbra => Some(Type::Umbra),
            TokenKind::Photon => Some(Type::Photon),
            TokenKind::Identifier(name) => Some(Type::Custom(name.clone())),
            _ => None,
        }
    }

    fn peek(&self) -> &Token {
        &self.current
    }

    fn get_rule(&self, kind: &TokenKind) -> ParseRule<'a> {
        use TokenKind::*;
        match kind {
            Bang => ParseRule {
                prefix: Some(Parser::parse_prefix_unary),
                infix: None,
                precedence: Precedence::None,
            },
            EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Equality,
            },
            BangEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Equality,
            },
            Greater => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Comparison,
            },
            GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Comparison,
            },
            Less => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Comparison,
            },
            LessEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Comparison,
            },
            Number(_) => ParseRule {
                prefix: Some(Parser::parse_number),
                infix: None,
                precedence: Precedence::None,
            },
            Plus => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Term,
            },
            Star => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Factor,
            },
            Minus => ParseRule {
                prefix: Some(Parser::parse_prefix_unary),
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Term,
            },
            Slash => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Factor,
            },
            Percent => ParseRule {
                prefix: None,
                infix: Some(Parser::parse_infix_binary),
                precedence: Precedence::Factor,
            },
            LeftParen => ParseRule {
                prefix: Some(Parser::parse_grouping),
                infix: None,
                precedence: Precedence::None,
            },
            True => ParseRule {
                prefix: Some(Parser::parse_literal),
                infix: None,
                precedence: Precedence::None,
            },
            False => ParseRule {
                prefix: Some(Parser::parse_literal),
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
