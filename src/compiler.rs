use crate::{
    ast::{Expr, Stmt},
    error::ParseError,
    lexer::Lexer,
    parser::Parser,
    value::Value,
};

#[derive(Debug, Clone)]
pub enum Op {
    Const(usize),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Not,
    Negate,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Pop,
    Print,
    Return,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
}

pub struct Compiler {
    pub ast: Vec<Stmt>,
}

impl Compiler {
    pub fn new(source: String) -> Result<Self, ParseError> {
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;

        Ok(Compiler { ast })
    }

    pub fn compile(&self) -> Chunk {
        let mut chunk = Chunk {
            code: Vec::new(),
            constants: Vec::new(),
        };

        for stmt in &self.ast {
            match stmt {
                Stmt::ExprStmt(expr) => {
                    Self::compile_expr(expr, &mut chunk);
                    // chunk.code.push(Op::Pop);
                }
                Stmt::EmitStmt(expr) => {
                    Self::compile_expr(expr, &mut chunk);
                    chunk.code.push(Op::Print);
                }
                _ => {
                    // Ignore let/const for now
                }
            }
        }

        // chunk.code.push(Op::Return);

        chunk
    }

    fn compile_expr(expr: &Expr, chunk: &mut Chunk) {
        match expr {
            Expr::Binary { left, op, right } => {
                Self::compile_expr(left, chunk);
                Self::compile_expr(right, chunk);

                match op {
                    crate::ast::BinaryOp::Plus => chunk.code.push(Op::Add),
                    crate::ast::BinaryOp::Minus => chunk.code.push(Op::Sub),
                    crate::ast::BinaryOp::Star => chunk.code.push(Op::Mul),
                    crate::ast::BinaryOp::Slash => chunk.code.push(Op::Div),
                    crate::ast::BinaryOp::Percent => chunk.code.push(Op::Rem),
                    crate::ast::BinaryOp::EqualEqual => chunk.code.push(Op::Equal),
                    crate::ast::BinaryOp::BangEqual => chunk.code.push(Op::NotEqual),
                    crate::ast::BinaryOp::Less => chunk.code.push(Op::Less),
                    crate::ast::BinaryOp::LessEqual => chunk.code.push(Op::LessEqual),
                    crate::ast::BinaryOp::Greater => chunk.code.push(Op::Greater),
                    crate::ast::BinaryOp::GreaterEqual => chunk.code.push(Op::GreaterEqual),
                }
            }

            Expr::Bool(b) => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Photon(*b));
                chunk.code.push(Op::Const(index));
            }

            Expr::String(s) => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Lumens(s.clone()));
                chunk.code.push(Op::Const(index));
            }

            Expr::Number(n) => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Light(*n));
                chunk.code.push(Op::Const(index));
            }

            Expr::Umbra => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Umbra);
                chunk.code.push(Op::Const(index));
            }

            Expr::Unary { op, expr } => {
                Self::compile_expr(expr, chunk);
                match op {
                    crate::ast::UnaryOp::Bang => chunk.code.push(Op::Not),
                    crate::ast::UnaryOp::Minus => chunk.code.push(Op::Negate),
                }
            }

            _ => {}
        }
    }
}
