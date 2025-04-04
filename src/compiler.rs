use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Expr, Stmt},
    error::ParseError,
    lexer::Lexer,
    parser::Parser,
    value::{Chunk, Function, Op, Value},
};

pub struct Compiler {
    scopes: Vec<HashMap<String, usize>>,
    next_slot: usize,
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
            scopes: vec![],
            next_slot: 0,
            ast,
            globals: HashMap::new(),
        })
    }

    pub fn compile(&mut self) -> Rc<Function> {
        let chunk = self.compile_stmt(self.ast.clone());

        Rc::new(Function {
            name: "_start".to_string(),
            arity: 0,
            chunk,
        })
    }

    fn compile_stmt(&mut self, stmts: Vec<Stmt>) -> Chunk {
        let mut chunk = Chunk {
            code: Vec::new(),
            constants: Vec::new(),
        };

        for stmt in stmts {
            match stmt {
                Stmt::LetDecl { name, value, .. } => {
                    Self::compile_expr(self, &value, &mut chunk);
                    let slot = self.declare_local(&name);
                    chunk.code.push(Op::SetLocal(slot));
                }
                Stmt::ConstDecl { name, value, .. } => {
                    Self::compile_expr(self, &value, &mut chunk);
                    chunk.code.push(Op::SetGlobal(name.clone()));
                }
                Stmt::ExprStmt(expr) => {
                    Self::compile_expr(self, &expr, &mut chunk);
                }
                Stmt::EmitStmt(expr) => {
                    Self::compile_expr(self, &expr, &mut chunk);
                    chunk.code.push(Op::Print);
                }
                Stmt::FnDecl {
                    name,
                    params,
                    arity,
                    body,
                    return_type,
                } => {
                    self.begin_scope();

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

                    self.end_scope();
                }
                _ => {
                    // Ignore let/const for now
                }
            }
        }

        chunk
    }

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) {
        match expr {
            Expr::Identifier(name) => {
                if let Some(slot) = self.resolve_local(&name) {
                    chunk.code.push(Op::GetLocal(slot));
                } else {
                    chunk.code.push(Op::GetGlobal(name.clone()));
                }
            }
            Expr::Binary { left, op, right } => {
                Self::compile_expr(self, left, chunk);
                Self::compile_expr(self, right, chunk);

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
                Self::compile_expr(self, expr, chunk);
                match op {
                    crate::ast::UnaryOp::Bang => chunk.code.push(Op::Not),
                    crate::ast::UnaryOp::Minus => chunk.code.push(Op::Negate),
                }
            }
            Expr::Call { callee, args } => {
                Self::compile_expr(self, callee, chunk);
                for arg in args {
                    Self::compile_expr(self, arg, chunk);
                }
                let name = match &**callee {
                    Expr::Identifier(name) => name.clone(),
                    _ => panic!("Expected identifier"),
                };
                chunk.code.push(Op::Call(name));
            }
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_local(&mut self, name: &str) -> usize {
        let index = self.next_slot;
        self.next_slot += 1;

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), index);

        index
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(&index) = scope.get(name) {
                return Some(index);
            }
        }

        None
    }
}
