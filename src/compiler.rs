use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{BinaryOp, Expr, LogicalOp, Stmt},
    error::{ParseError, PrismError, PrismResult},
    lexer::Lexer,
    parser::Parser,
    value::{Chunk, Function, Op, Value},
};

#[derive(Clone, Debug)]
struct LoopContext {
    start: usize,               // where to jump to on continue
    break_jumps: Vec<usize>,    // places to patch at loop exit
    continue_jumps: Vec<usize>, // places to patch before post
}

#[derive(Clone, Debug)]
pub struct Compiler {
    filename: String,
    ast: Vec<Stmt>,
    pub globals: HashMap<String, Value>,
    pub context: CompilerContext,
    pub enclosing: Option<Rc<RefCell<Compiler>>>,
    loop_stack: Vec<LoopContext>,
}

impl Compiler {
    pub fn new(filename: &str, source: String) -> Result<Rc<RefCell<Self>>, ParseError> {
        let lexer = Lexer::new(&source, filename);
        let mut parser = Parser::new(filename, lexer);
        let ast = parser.parse()?;

        let _start = Rc::new(RefCell::new(Function {
            name: "_start".to_string(),
            arity: 0,
            chunk: Chunk {
                code: Vec::new(),
                constants: Vec::new(),
            },
            upvalue_count: 0,
        }));

        let context = CompilerContext {
            function: _start,
            upvalues: vec![],
            scopes: vec![HashMap::new()],
            is_global_scope: true,
            next_slot: 0,
        };

        Ok(Rc::new(RefCell::new(Compiler {
            filename: filename.to_string(),
            ast,
            globals: HashMap::new(),
            context,
            enclosing: None,
            loop_stack: Vec::new(),
        })))
    }

    pub fn compile(&mut self) -> PrismResult<Rc<RefCell<Function>>> {
        self.compile_stmts(&self.ast.clone())?;

        // Look for Prism() in globals
        if self.globals.contains_key("Prism") {
            self.emit(Op::GetGlobal("Prism".to_string()));
            self.emit(Op::Call(0));
        } else {
            return Err(PrismError::Compile("Prism() not found".to_string()));
        }

        // Return Umbra (implicitly)
        let index = self.add_constant(Value::Umbra);
        self.emit(Op::Constant(index));
        self.emit(Op::Return);

        Ok(self.context.function.clone())
    }

    fn compile_stmts(&mut self, stmts: &[Stmt]) -> Result<(), PrismError> {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), PrismError> {
        match stmt {
            Stmt::LetDecl { name, value, .. } => {
                if self.context.is_global_scope {
                    return Err(PrismError::Compile(
                        " `let` is not allowed at global scope".to_string(),
                    ));
                }

                if let Some(slot) = self.context.resolve_local(name) {
                    match self.context.function.borrow().chunk.constants[slot] {
                        Value::Function(_) => {}
                        _ => {
                            return Err(PrismError::Compile(format!(
                                "Variable '{}' already declared in this scope",
                                name
                            )));
                        }
                    }
                }

                Self::compile_expr(self, value)?;
                let slot = self.context.declare_local(name);
                self.emit(Op::SetLocal(slot));
            }
            Stmt::ConstDecl { name, value, .. } => {
                if !self.context.is_global_scope {
                    return Err(PrismError::Compile(
                        " `const` is not allowed at local scope".to_string(),
                    ));
                }

                if self.globals.contains_key(name) {
                    return Err(PrismError::Compile(format!(
                        "Constant '{}' already declared in this scope",
                        name
                    )));
                }

                Self::compile_expr(self, value)?;
                self.emit(Op::SetGlobal(name.clone()));
            }
            Stmt::Expr(expr) => {
                Self::compile_expr(self, expr)?;
            }
            Stmt::Emit(expr) => {
                Self::compile_expr(self, expr)?;
                self.emit(Op::Print);
            }
            Stmt::FnDecl {
                name,
                arity,
                body,
                params,
                ..
            } => {
                if self.context.resolve_local(name).is_some() || self.globals.contains_key(name) {
                    return Err(PrismError::Compile(format!(
                        "Function '{}' already declared in this scope",
                        name
                    )));
                }

                let function = Rc::new(RefCell::new(Function {
                    name: name.clone(),
                    arity: *arity,
                    chunk: Chunk {
                        code: Vec::new(),
                        constants: Vec::new(),
                    },
                    upvalue_count: 0,
                }));

                let parent = Rc::new(RefCell::new(self.clone()));

                let nested = Rc::new(RefCell::new(Compiler {
                    filename: self.filename.clone(),
                    ast: body.clone(),
                    globals: self.globals.clone(),
                    context: CompilerContext {
                        function: function.clone(),
                        upvalues: vec![],
                        scopes: vec![HashMap::new()],
                        is_global_scope: false,
                        next_slot: 0,
                    },
                    enclosing: Some(parent),
                    loop_stack: Vec::new(),
                }));

                for (param_name, _) in params.iter() {
                    nested.borrow_mut().context.declare_local(param_name);
                }

                nested.borrow_mut().context.begin_scope();
                nested.borrow_mut().compile_stmts(body)?;
                nested.borrow_mut().context.end_scope();

                let index = nested.borrow_mut().add_constant(Value::Umbra);
                nested.borrow_mut().emit(Op::Constant(index));
                nested.borrow_mut().emit(Op::Return);

                let value = Value::Function(function.clone());

                if self.context.is_global_scope {
                    self.globals.insert(name.clone(), value.clone());
                } else {
                    let fn_index = self.add_constant(value.clone());

                    let slot = self.context.declare_local(name);

                    let upvalues: Vec<(bool, usize)> = nested
                        .borrow()
                        .context
                        .upvalues
                        .iter()
                        .map(|u| (u.is_local, u.index))
                        .collect();

                    self.emit(Op::Closure { fn_index, upvalues });
                    self.emit(Op::SetLocal(slot));
                }
            }
            Stmt::For {
                init,
                condition,
                post,
                body,
            } => {
                if let Some(init) = init {
                    self.compile_stmt(init)?;
                }

                let mut loop_ctx = LoopContext {
                    start: 0,
                    break_jumps: vec![],
                    continue_jumps: vec![],
                };

                let cond_jump = if let Some(condition) = condition {
                    let cond_start = self.current_ip();
                    loop_ctx.start = cond_start;
                    self.compile_expr(condition)?;
                    let exit_jump = self.current_ip();
                    self.emit(Op::JumpIfFalse(usize::MAX));
                    self.emit(Op::Pop);
                    Some(exit_jump)
                } else {
                    let start = self.current_ip();
                    loop_ctx.start = start;
                    None
                };

                self.loop_stack.push(loop_ctx.clone());

                self.context.begin_scope();
                self.compile_stmts(body)?;
                self.context.end_scope();

                let post_start = self.current_ip();

                if let Some(post) = post {
                    self.compile_expr(post)?;
                    self.emit(Op::Pop);
                }

                self.emit(Op::Jump(loop_ctx.start));

                let end = self.current_ip();
                let ctx = self.loop_stack.pop().unwrap();

                // Patch break and continue jumps
                for jump in ctx.break_jumps {
                    self.emit_at(Op::Jump(end), jump);
                }
                for jump in ctx.continue_jumps {
                    self.emit_at(Op::Jump(post_start), jump);
                }

                if let Some(cond_jump) = cond_jump {
                    self.emit_at(Op::JumpIfFalse(end), cond_jump);
                }
            }

            Stmt::Switch {
                target,
                cases,
                default,
            } => {
                self.compile_expr(target)?;

                let mut end_jumps = Vec::new();

                for (match_expr, body) in cases {
                    self.emit(Op::Dup); // duplicate switch target
                    self.compile_expr(match_expr)?;
                    self.emit(Op::Equal);

                    let jump_if_false_pos = self.current_ip();
                    self.emit(Op::JumpIfFalse(usize::MAX));
                    self.emit(Op::Pop);

                    self.compile_stmts(body)?;
                    end_jumps.push(self.current_ip());
                    self.emit(Op::Jump(usize::MAX));

                    self.emit_at(Op::JumpIfFalse(self.current_ip()), jump_if_false_pos);
                    self.emit(Op::Pop);
                }

                if let Some(default_block) = default {
                    self.emit(Op::Pop);
                    self.compile_stmts(default_block)?;
                } else {
                    self.emit(Op::Pop);
                }

                let end = self.current_ip();
                for pos in end_jumps {
                    self.emit_at(Op::Jump(end), pos);
                }
            }

            Stmt::If {
                condition,
                body,
                else_body,
            } => {
                Self::compile_expr(self, condition)?;

                let else_pos = self.current_ip();
                self.emit(Op::JumpIfFalse(usize::MAX));

                self.emit(Op::Pop);

                self.context.begin_scope();
                self.compile_stmts(body)?;
                self.context.end_scope();

                let jump_pos = self.current_ip();
                self.emit(Op::Jump(usize::MAX));

                let else_start = self.current_ip();
                self.emit_at(Op::JumpIfFalse(else_start), else_pos);

                self.emit(Op::Pop);

                if let Some(else_body) = else_body {
                    self.context.begin_scope();
                    self.compile_stmts(else_body)?;
                    self.context.end_scope();
                }

                let after = self.current_ip();
                self.emit_at(Op::Jump(after), jump_pos);
            }
            Stmt::Break => {
                if let Some(ctx) = self.loop_stack.last() {
                    let jump_pos = self.current_ip();
                    self.emit(Op::Jump(usize::MAX));
                    self.loop_stack
                        .last_mut()
                        .unwrap()
                        .break_jumps
                        .push(jump_pos);
                } else {
                    return Err(PrismError::Compile("`break` outside of loop".into()));
                }
            }

            Stmt::Continue => {
                if let Some(ctx) = self.loop_stack.last() {
                    let jump_pos = self.current_ip();
                    self.emit(Op::Jump(usize::MAX));
                    self.loop_stack
                        .last_mut()
                        .unwrap()
                        .continue_jumps
                        .push(jump_pos);
                } else {
                    return Err(PrismError::Compile("`continue` outside of loop".into()));
                }
            }

            Stmt::Return(expr_opt) => {
                match expr_opt {
                    Some(expr) => {
                        Self::compile_expr(self, expr)?;
                    }
                    None => {
                        let index = self.add_constant(Value::Umbra);
                        self.emit(Op::Constant(index));
                    }
                }

                self.emit(Op::Return);
            }
            _ => {}
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), PrismError> {
        match expr {
            Expr::Array { elements } => {
                for element in elements {
                    self.compile_expr(element)?;
                }
                self.emit(Op::MakeArray(elements.len()));
            }

            Expr::AssignIndex {
                array,
                index,
                value,
            } => {
                self.compile_expr(array)?;
                self.compile_expr(index)?;
                self.compile_expr(value)?;
                self.emit(Op::ArraySet);
            }

            Expr::Index { array, index } => {
                self.compile_expr(array)?;
                self.compile_expr(index)?;
                self.emit(Op::ArrayGet);
            }

            Expr::Identifier(name) => match self.context.resolve_local(name) {
                Some(slot) => self.emit(Op::GetLocal(slot)),
                None => match self.resolve_upvalue(name) {
                    Some(index) => self.emit(Op::GetUpvalue(index)),
                    None => self.emit(Op::GetGlobal(name.clone())),
                },
            },

            Expr::Binary { left, op, right } => {
                Self::compile_expr(self, left)?;
                Self::compile_expr(self, right)?;

                self.emit(Self::map_binary_op(op.clone()));
            }

            Expr::Bool(b) => {
                let index = self.add_constant(Value::Photon(*b));
                self.emit(Op::Constant(index));
            }

            Expr::String(s) => {
                let index = self.add_constant(Value::Lumens(s.clone()));
                self.emit(Op::Constant(index));
            }

            Expr::Number(n) => {
                let index = self.add_constant(Value::Light(*n));
                self.emit(Op::Constant(index));
            }

            Expr::Umbra => {
                let index = self.add_constant(Value::Umbra);
                self.emit(Op::Constant(index));
            }

            Expr::Unary { op, expr } => {
                Self::compile_expr(self, expr)?;
                match op {
                    crate::ast::UnaryOp::Bang => self.emit(Op::Not),
                    crate::ast::UnaryOp::Minus => self.emit(Op::Negate),
                }
            }

            Expr::Assign { name, value } => {
                if let Expr::Assign { .. } = **value {
                    return Err(PrismError::Compile(
                        "Chained assignment is not allowed".to_string(),
                    ));
                }

                Self::compile_expr(self, value)?;

                match self.context.resolve_local(name) {
                    Some(slot) => {
                        self.emit(Op::SetLocal(slot));
                        self.emit(Op::GetLocal(slot));
                    }
                    None => match self.resolve_upvalue(name) {
                        Some(index) => {
                            self.emit(Op::SetUpvalue(index));
                            self.emit(Op::GetUpvalue(index));
                        }
                        None => {
                            return Err(PrismError::Compile(format!(
                                "Cannot assign to '{}' constant",
                                name
                            )));
                        }
                    },
                }
            }

            Expr::AssignOp { name, op, value } => match self.context.resolve_local(name) {
                Some(slot) => {
                    self.emit(Op::GetLocal(slot));
                    Self::compile_expr(self, value)?;
                    self.emit(Self::map_binary_op(op.clone()));
                    self.emit(Op::SetLocal(slot));
                    self.emit(Op::GetLocal(slot));
                }
                None => match self.resolve_upvalue(name) {
                    Some(index) => {
                        self.emit(Op::GetUpvalue(index));
                        Self::compile_expr(self, value)?;
                        self.emit(Self::map_binary_op(op.clone()));
                        self.emit(Op::SetUpvalue(index));
                        self.emit(Op::GetUpvalue(index));
                    }
                    None => {
                        return Err(PrismError::Compile(format!(
                            "Undefined variable '{}'",
                            name
                        )));
                    }
                },
            },

            Expr::Logical { left, op, right } => {
                Self::compile_expr(self, left)?;

                match op {
                    LogicalOp::OrOr => {
                        let jump_pos = self.current_ip();
                        self.emit(Op::JumpIfFalse(usize::MAX));
                        self.emit(Op::Jump(usize::MAX));

                        // Patch the jump if false
                        let after = self.current_ip();
                        self.emit_at(Op::JumpIfFalse(after), jump_pos);

                        self.emit(Op::Pop);
                        Self::compile_expr(self, right)?;

                        let after = self.current_ip();
                        self.emit_at(Op::Jump(after), jump_pos + 1);
                    }
                    LogicalOp::AndAnd => {
                        let jump_pos = self.current_ip();
                        self.emit(Op::JumpIfFalse(usize::MAX));

                        self.emit(Op::Pop);
                        Self::compile_expr(self, right)?;

                        // Patch the jump if false
                        let after = self.current_ip();
                        self.emit_at(Op::JumpIfFalse(after), jump_pos);
                    }
                }
            }

            Expr::Call { callee, args } => {
                if let Expr::Identifier(name) = &**callee {
                    let expected_arity = match self.globals.get(name) {
                        Some(Value::Function(func)) => Some(func.borrow().arity),
                        _ => self.context.resolve_local(name).and_then(|slot| {
                            match self.context.function.borrow().chunk.constants.get(slot) {
                                Some(Value::Function(func)) => Some(func.borrow().arity),
                                _ => None,
                            }
                        }),
                    };

                    if let Some(expected) = expected_arity {
                        if expected != args.len() {
                            return Err(PrismError::Compile(format!(
                                "Function '{}' expects {} arguments, but got {}",
                                name,
                                expected,
                                args.len()
                            )));
                        }
                    }
                }

                for arg in args {
                    Self::compile_expr(self, arg)?;
                }

                Self::compile_expr(self, callee)?;
                self.emit(Op::Call(args.len()));
            }

            Expr::Lambda {
                params,
                body,
                arity,
                ..
            } => {
                let function = Rc::new(RefCell::new(Function {
                    name: "<lambda>".to_string(),
                    arity: *arity,
                    chunk: Chunk {
                        code: Vec::new(),
                        constants: Vec::new(),
                    },
                    upvalue_count: 0,
                }));

                let parent = Rc::new(RefCell::new(self.clone()));

                let nested = Rc::new(RefCell::new(Compiler {
                    filename: self.filename.clone(),
                    ast: body.clone(),
                    globals: self.globals.clone(),
                    context: CompilerContext {
                        function: function.clone(),
                        upvalues: vec![],
                        scopes: vec![HashMap::new()],
                        is_global_scope: false,
                        next_slot: 0,
                    },
                    enclosing: Some(parent),
                    loop_stack: vec![],
                }));

                for (param_name, _) in params.iter() {
                    nested.borrow_mut().context.declare_local(param_name);
                }

                nested.borrow_mut().context.begin_scope();
                nested.borrow_mut().compile_stmts(body)?;
                nested.borrow_mut().context.end_scope();

                let index = nested.borrow_mut().add_constant(Value::Umbra);
                nested.borrow_mut().emit(Op::Constant(index));
                nested.borrow_mut().emit(Op::Return);

                let fn_index = self.add_constant(Value::Function(function.clone()));
                let upvalues = nested
                    .borrow()
                    .context
                    .upvalues
                    .iter()
                    .map(|u| (u.is_local, u.index))
                    .collect();

                self.emit(Op::Closure { fn_index, upvalues });
            }
        }

        Ok(())
    }

    fn map_binary_op(op: BinaryOp) -> Op {
        match op {
            BinaryOp::Plus => Op::Add,
            BinaryOp::Minus => Op::Sub,
            BinaryOp::Star => Op::Mul,
            BinaryOp::Slash => Op::Div,
            BinaryOp::Percent => Op::Rem,
            BinaryOp::EqualEqual => Op::Equal,
            BinaryOp::BangEqual => Op::NotEqual,
            BinaryOp::Less => Op::Less,
            BinaryOp::LessEqual => Op::LessEqual,
            BinaryOp::Greater => Op::Greater,
            BinaryOp::GreaterEqual => Op::GreaterEqual,
        }
    }

    pub fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        if let Some(enclosing_rc) = &self.enclosing {
            let mut enclosing = enclosing_rc.borrow_mut();

            if let Some(local_index) = enclosing.context.resolve_local(name) {
                return Some(self.context.add_upvalue(true, local_index));
            }

            if let Some(up_index) = enclosing.resolve_upvalue(name) {
                return Some(self.context.add_upvalue(false, up_index));
            }
        }

        None
    }

    fn current_ip(&self) -> usize {
        self.context.function.borrow().chunk.code.len()
    }

    fn emit(&mut self, op: Op) {
        self.context.function.borrow_mut().chunk.code.push(op);
    }

    fn emit_at(&mut self, op: Op, index: usize) {
        self.context.function.borrow_mut().chunk.code[index] = op;
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.context
            .function
            .borrow_mut()
            .chunk
            .constants
            .push(value);
        self.context.function.borrow_mut().chunk.constants.len() - 1
    }
}

#[derive(Debug, Clone, PartialEq)]
struct UpvalueInfo {
    is_local: bool,
    index: usize,
}

#[derive(Clone, Debug)]
pub struct CompilerContext {
    pub function: Rc<RefCell<Function>>,
    upvalues: Vec<UpvalueInfo>,
    scopes: Vec<HashMap<String, usize>>,
    is_global_scope: bool,
    next_slot: usize,
}

impl CompilerContext {
    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_local(&mut self, name: &str) -> usize {
        let index = self.next_slot;
        self.next_slot += 1;

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), index);

        index
    }

    pub fn resolve_local(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(&index) = scope.get(name) {
                return Some(index);
            }
        }
        None
    }

    pub fn add_upvalue(&mut self, is_local: bool, index: usize) -> usize {
        let up = UpvalueInfo { is_local, index };

        if let Some(i) = self.upvalues.iter().position(|u| *u == up) {
            return i;
        }

        self.upvalues.push(up);
        self.function.borrow_mut().upvalue_count += 1;
        self.upvalues.len() - 1
    }
}
