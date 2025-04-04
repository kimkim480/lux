use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Expr, Stmt},
    error::ParseError,
    lexer::Lexer,
    parser::Parser,
    value::{Chunk, Function, Op, Value},
};

pub struct Compiler {
    pub ast: Vec<Stmt>,
    pub globals: HashMap<String, Value>,
}

impl Compiler {
    pub fn new(source: String) -> Result<Self, ParseError> {
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;

        // println!("🔹 [ast] {:?}", ast);

        Ok(Compiler {
            ast,
            globals: HashMap::new(),
        })
    }

    pub fn compile(&mut self) -> Chunk {
        self.compile_stmt(self.ast.clone())
    }

    fn compile_stmt(&mut self, stmts: Vec<Stmt>) -> Chunk {
        let mut chunk = Chunk {
            code: Vec::new(),
            constants: Vec::new(),
        };

        for stmt in stmts {
            match stmt {
                Stmt::ConstDecl { name, value, .. } => {
                    Self::compile_expr(&value, &mut chunk);
                    chunk.code.push(Op::SetGlobal(name.clone()));
                }
                Stmt::ExprStmt(expr) => {
                    Self::compile_expr(&expr, &mut chunk);
                    // chunk.code.push(Op::Pop);
                }
                Stmt::EmitStmt(expr) => {
                    Self::compile_expr(&expr, &mut chunk);
                    chunk.code.push(Op::Print);
                }
                Stmt::FnDecl {
                    name,
                    params,
                    arity,
                    body,
                    return_type,
                } => {
                    let mut fn_chunk = self.compile_stmt(body);
                    fn_chunk.constants.push(Value::Umbra);
                    fn_chunk.code.push(Op::Return);

                    let func = Function {
                        name: name.clone(),
                        arity,
                        chunk: fn_chunk,
                    };

                    // Store in global as const
                    let value = Value::Function(Rc::new(func));
                    self.globals.insert(name.clone(), value);
                }
                _ => {
                    // Ignore let/const for now
                }
            }
        }

        chunk
    }

    fn compile_expr(expr: &Expr, chunk: &mut Chunk) {
        match expr {
            Expr::Identifier(name) => {
                chunk.code.push(Op::GetGlobal(name.clone()));
            }
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
                chunk.code.push(Op::Constant(index));
            }

            Expr::String(s) => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Lumens(s.clone()));
                chunk.code.push(Op::Constant(index));
            }

            Expr::Number(n) => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Light(*n));
                chunk.code.push(Op::Constant(index));
            }

            Expr::Umbra => {
                let index = chunk.constants.len();
                chunk.constants.push(Value::Umbra);
                chunk.code.push(Op::Constant(index));
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
