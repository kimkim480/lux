use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    bytecode::Op,
    error::{PrismError, PrismResult},
    get_source_line,
    syntax::{
        Module,
        ast::{BinaryOp, Expr, LogicalOp, Spanned, Stmt, UnaryOp},
    },
    types::LuxType,
    vm::value::{Chunk, Function, TypeDef, Value},
};

#[derive(Clone, Debug)]
pub struct MethodInfo {
    pub value: Value,
    pub is_static: bool,
}

#[derive(Clone, Debug)]
struct LoopContext {
    start: usize,               // where to jump to on continue
    break_jumps: Vec<usize>,    // places to patch at loop exit
    continue_jumps: Vec<usize>, // places to patch before post
}

#[derive(Clone, Debug)]
pub struct Compiler {
    module: Module,
    pub globals: HashMap<String, Value>,
    pub context: CompilerContext,
    pub enclosing: Option<Rc<RefCell<Compiler>>>,
    loop_stack: Vec<LoopContext>,
    pub type_defs: HashMap<String, TypeDef>,
    pub refraction_methods: HashMap<(String, String), MethodInfo>,
    source: String,
}

impl Compiler {
    pub fn new(source: &String, module: Module) -> Rc<RefCell<Self>> {
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

        Rc::new(RefCell::new(Compiler {
            module,
            globals: HashMap::new(),
            context,
            enclosing: None,
            loop_stack: Vec::new(),
            type_defs: HashMap::new(),
            refraction_methods: HashMap::new(),
            source: source.clone(),
        }))
    }

    pub fn compile(&mut self) -> PrismResult<Rc<RefCell<Function>>> {
        self.compile_stmts(&self.module.body.clone())?;

        // Look for Prism() in globals
        if self.globals.contains_key("Prism") {
            self.emit(Op::GetGlobal("Prism".to_string()));
            self.emit(Op::Call(0));
        } else {
            return Err(PrismError::compile_error(
                "Prism() not found",
                &self.module.body[0].span,
                &get_source_line(&self.source, self.module.body[0].span.line),
            ));
        }

        // Return Umbra (implicitly)
        let index = self.add_constant(Value::Umbra);
        self.emit(Op::Constant(index));
        self.emit(Op::Return);

        Ok(self.context.function.clone())
    }

    fn compile_stmts(&mut self, stmts: &[Spanned<Stmt>]) -> Result<(), PrismError> {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<(), PrismError> {
        match &stmt.node {
            Stmt::ConstellationDecl(_) => {}

            Stmt::LetDecl { name, value, ty } => {
                if self.context.is_global_scope {
                    return Err(PrismError::compile_error(
                        " `let` is not allowed at global scope",
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                if let Some(_) = self.context.resolve_local(name) {
                    return Err(PrismError::compile_error(
                        format!("Variable '{}' already declared in this scope", name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                if let Some(value) = value {
                    Self::compile_expr(self, value)?;
                    let slot = self.context.declare_local(name, value.node.clone());
                    self.emit(Op::SetLocal(slot));
                } else {
                    let zero_value = self.get_zero_value(ty);
                    let slot = self
                        .context
                        .declare_local(name, Expr::Identifier(zero_value.to_string()));
                    let index = self.add_constant(zero_value);
                    self.emit(Op::Constant(index));
                    self.emit(Op::SetLocal(slot));
                }
            }

            Stmt::ConstDecl { name, value, .. } => {
                if !self.context.is_global_scope {
                    return Err(PrismError::compile_error(
                        " `const` is not allowed at local scope",
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                if self.globals.contains_key(name) {
                    return Err(PrismError::compile_error(
                        format!("Constant '{}' already declared in this scope", name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                Self::compile_expr(self, value)?;
                self.emit(Op::SetGlobal(name.clone()));
            }

            Stmt::FacetDecl { .. } => {} // handled in typechecker

            Stmt::TypeAlias { .. } => {} // handled in typechecker

            Stmt::Expr(expr) => {
                Self::compile_expr(self, expr)?;
            }

            Stmt::Emit(expr) => {
                Self::compile_expr(self, expr)?;
                self.emit(Op::Print);
            }

            Stmt::RadiateDecl {
                facet_name,
                methods,
            } => {
                if !self.context.is_global_scope {
                    return Err(PrismError::compile_error(
                        " `Radiate` declarations must be at global scope",
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                for method in methods {
                    let function = Rc::new(RefCell::new(Function {
                        name: method.name.clone(),
                        arity: method.arity,
                        chunk: Chunk {
                            code: Vec::new(),
                            constants: Vec::new(),
                        },
                        upvalue_count: 0,
                    }));

                    let parent = Rc::new(RefCell::new(self.clone()));
                    let nested = Rc::new(RefCell::new(Compiler {
                        source: self.source.clone(),
                        module: Module {
                            body: method.body.clone(),
                        },
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
                        type_defs: self.type_defs.clone(),
                        refraction_methods: self.refraction_methods.clone(),
                    }));

                    for (param_name, _) in &method.params {
                        nested
                            .borrow_mut()
                            .context
                            .declare_local(param_name, Expr::Identifier(param_name.to_string()));
                    }

                    nested.borrow_mut().context.begin_scope();
                    nested.borrow_mut().compile_stmts(&method.body)?;
                    nested.borrow_mut().context.end_scope();

                    let index = nested.borrow_mut().add_constant(Value::Umbra);
                    nested.borrow_mut().emit(Op::Constant(index));
                    nested.borrow_mut().emit(Op::Return);

                    let value = Value::Function(function.clone());
                    self.refraction_methods.insert(
                        (facet_name.clone(), method.name.clone()),
                        MethodInfo {
                            value,
                            is_static: method.is_static,
                        },
                    );
                }
            }

            Stmt::FnDecl {
                name,
                arity,
                body,
                params,
                ..
            } => {
                if self.context.resolve_local(name).is_some() || self.globals.contains_key(name) {
                    return Err(PrismError::compile_error(
                        format!("Function '{}' already declared in this scope", name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                let slot = if self.context.is_global_scope {
                    0 // dummy; not used in global scope
                } else {
                    self.context
                        .declare_local(name, Expr::Identifier(name.to_string()))
                };

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
                    source: self.source.clone(),
                    module: Module { body: body.clone() },
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
                    type_defs: self.type_defs.clone(),
                    refraction_methods: self.refraction_methods.clone(),
                }));

                for (param_name, _) in params.iter() {
                    nested
                        .borrow_mut()
                        .context
                        .declare_local(param_name, Expr::Identifier(param_name.to_string()));
                }

                nested.borrow_mut().context.begin_scope();
                nested.borrow_mut().compile_stmts(body)?;
                nested.borrow_mut().context.end_scope();

                // implicitly return Umbra
                let index = nested.borrow_mut().add_constant(Value::Umbra);
                nested.borrow_mut().emit(Op::Constant(index));
                nested.borrow_mut().emit(Op::Return);

                let value = Value::Function(function.clone());

                if self.context.is_global_scope {
                    self.globals.insert(name.clone(), value.clone());
                } else {
                    let fn_index = self.add_constant(value);

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
                self.context.begin_scope();

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

                self.compile_stmts(body)?;

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

                self.context.end_scope();
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
                if let Some(_) = self.loop_stack.last() {
                    let jump_pos = self.current_ip();
                    self.emit(Op::Jump(usize::MAX));
                    self.loop_stack
                        .last_mut()
                        .unwrap()
                        .break_jumps
                        .push(jump_pos);
                } else {
                    return Err(PrismError::compile_error(
                        "`break` outside of loop",
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
            }

            Stmt::Continue => {
                if let Some(_) = self.loop_stack.last() {
                    let jump_pos = self.current_ip();
                    self.emit(Op::Jump(usize::MAX));
                    self.loop_stack
                        .last_mut()
                        .unwrap()
                        .continue_jumps
                        .push(jump_pos);
                } else {
                    return Err(PrismError::compile_error(
                        "`continue` outside of loop",
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
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
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Spanned<Expr>) -> Result<(), PrismError> {
        match &expr.node {
            Expr::Map(key_value_pairs) => {
                for (key, value) in key_value_pairs {
                    self.compile_expr(key)?;
                    self.compile_expr(value)?;
                }
                self.emit(Op::MakeMap(key_value_pairs.len()));
            }
            Expr::Slice { callee, range } => {
                self.compile_expr(callee)?;
                self.compile_expr(range)?;

                match &callee.node {
                    Expr::Array { .. } => {
                        self.emit(Op::ArraySlice);
                    }
                    Expr::String { .. } => {
                        self.emit(Op::StringSlice);
                    }
                    Expr::Identifier(name) => match self.context.resolve_local(name) {
                        Some((_, value)) => match value {
                            Expr::String { .. } => self.emit(Op::StringSlice),
                            Expr::Array { .. } => self.emit(Op::ArraySlice),
                            _ => {
                                return Err(PrismError::compile_error(
                                    "Cannot slice non-string or non-array type",
                                    &expr.span,
                                    &get_source_line(&self.source, expr.span.line),
                                ));
                            }
                        },
                        None => match self.resolve_upvalue(name) {
                            Some(index) => self.emit(Op::GetUpvalue(index)),
                            None => self.emit(Op::GetGlobal(name.clone())),
                        },
                    },
                    _ => {
                        return Err(PrismError::compile_error(
                            "Cannot slice non-array type",
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    }
                }
            }

            Expr::Range { start, end } => {
                self.compile_expr(start)?;
                self.compile_expr(end)?;
                self.emit(Op::Range);
            }

            Expr::FacetInit { type_name, fields } => {
                // 1. Check that the type exists
                let expected_fields = match self.type_defs.get(type_name) {
                    Some(TypeDef::Facet { fields }) => fields.clone(),
                    _ => {
                        return Err(PrismError::compile_error(
                            format!("Type '{}' not found or not a Facet", type_name),
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    }
                };

                // 2. Compile the field expressions in the declared order
                for (decl_name, _) in &expected_fields {
                    let Some((_, expr)) = fields.iter().find(|(n, _)| n == decl_name) else {
                        return Err(PrismError::compile_error(
                            format!("Missing field '{}' in '{}'", decl_name, type_name),
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    };
                    self.compile_expr(expr)?;
                }

                // 3. Emit an Op to construct the facet
                self.emit(Op::MakeFacet {
                    type_name: type_name.clone(),
                    field_count: expected_fields.len(),
                });
            }

            Expr::FieldGet { object, field } => {
                self.compile_expr(object)?;
                self.emit(Op::FieldGet(field.clone()));
            }

            Expr::Array { elements } => {
                for element in elements {
                    self.compile_expr(element)?;
                }
                self.emit(Op::MakeArray(elements.len()));
            }

            Expr::AssignIndex {
                callee,
                index,
                value,
            } => {
                // A hack that helps in cases like `arr[i] = function(arr[i]);`
                let tmp_name = format!("__tmp__{}", self.context.next_slot);
                let value_slot = self.context.declare_local(&tmp_name, value.node.clone());

                self.compile_expr(value)?;
                self.emit(Op::SetLocal(value_slot));

                self.compile_expr(callee)?;
                self.compile_expr(index)?;

                self.emit(Op::GetLocal(value_slot));
                self.emit(Op::ArraySet);

                self.emit(Op::GetLocal(value_slot));
            }

            Expr::Index { callee, index } => {
                self.compile_expr(callee)?;
                self.compile_expr(index)?;
                self.emit(Op::ArrayIndex);
            }

            Expr::Identifier(name) => match self.context.resolve_local(name) {
                Some((slot, _)) => self.emit(Op::GetLocal(slot)),
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

            Expr::Boolean(b) => {
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
                    UnaryOp::Bang => self.emit(Op::Not),
                    UnaryOp::Minus => self.emit(Op::Negate),
                }
            }

            Expr::Assign { name, value } => {
                if let Expr::Assign { .. } = value.node {
                    return Err(PrismError::compile_error(
                        "Chained assignment is not allowed",
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

                Self::compile_expr(self, value)?;

                match self.context.resolve_local(name) {
                    Some((slot, _)) => {
                        self.emit(Op::SetLocal(slot));
                        self.emit(Op::GetLocal(slot));
                    }
                    None => match self.resolve_upvalue(name) {
                        Some(index) => {
                            self.emit(Op::SetUpvalue(index));
                            self.emit(Op::GetUpvalue(index));
                        }
                        None => {
                            return Err(PrismError::compile_error(
                                format!("Cannot assign to '{}' constant", name),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    },
                }
            }

            Expr::AssignOp { name, op, value } => match self.context.resolve_local(name) {
                Some((slot, _)) => {
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
                        return Err(PrismError::compile_error(
                            format!("Undefined variable '{}'", name),
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
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

            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                for arg in args {
                    self.compile_expr(arg)?;
                }

                match method.as_str() {
                    "len" => {
                        self.compile_expr(receiver)?;
                        self.emit(Op::Len);
                        return Ok(());
                    }
                    "push" => {
                        self.compile_expr(receiver)?;
                        self.emit(Op::ArrayPush);
                        return Ok(());
                    }
                    "pop" => {
                        self.compile_expr(receiver)?;
                        self.emit(Op::ArrayPop);
                        return Ok(());
                    }
                    _ => {}
                }

                if let Expr::Identifier(type_name) = &receiver.node {
                    if let Some(TypeDef::Facet { .. }) = self.type_defs.get(type_name) {
                        let key = (type_name.clone(), method.clone());
                        if let Some(info) = self.refraction_methods.get(&key) {
                            if info.is_static {
                                let cst = self.add_constant(info.value.clone());
                                self.emit(Op::Constant(cst));
                                self.emit(Op::Call(args.len()));
                                return Ok(());
                            }
                        } else {
                            return Err(PrismError::compile_error(
                                format!("Method '{}' not found on facet '{}'", method, type_name),
                                &receiver.span,
                                &get_source_line(&self.source, receiver.span.line),
                            ));
                        }
                    }
                }

                self.compile_expr(receiver)?;
                self.emit(Op::GetMethod(method.clone()));
                self.emit(Op::Call(args.len() + 1));
            }

            Expr::Call { callee, args } => {
                if let Expr::Identifier(name) = &callee.node {
                    let expected_arity = match self.globals.get(name) {
                        Some(Value::Function(func)) => Some(func.borrow().arity),
                        _ => self.context.resolve_local(name).and_then(|(slot, _)| {
                            match self.context.function.borrow().chunk.constants.get(slot) {
                                Some(Value::Function(func)) => Some(func.borrow().arity),
                                _ => None,
                            }
                        }),
                    };

                    if let Some(expected) = expected_arity {
                        if expected != args.len() {
                            return Err(PrismError::compile_error(
                                format!(
                                    "Function '{}' expects {} arguments, but got {}",
                                    name,
                                    expected,
                                    args.len()
                                ),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
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
                    source: self.source.clone(),
                    module: Module { body: body.clone() },
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
                    type_defs: self.type_defs.clone(),
                    refraction_methods: self.refraction_methods.clone(),
                }));

                for (param_name, _) in params.iter() {
                    nested
                        .borrow_mut()
                        .context
                        .declare_local(param_name, Expr::Identifier(param_name.to_string()));
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
            BinaryOp::StarStar => Op::Pow,
        }
    }

    pub fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        if let Some(enclosing_rc) = &self.enclosing {
            let mut enclosing = enclosing_rc.borrow_mut();

            if let Some((local_index, _)) = enclosing.context.resolve_local(name) {
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

    /// Returns the “zero” (default-initialized) runtime Value for a Lux type.
    /// Used when a `let` variable is declared without an explicit initializer.
    fn get_zero_value(&self, ty: &LuxType) -> Value {
        match ty {
            // ─ primitive types ────────────────────────────
            LuxType::Light => Value::Light(0.0),
            LuxType::Photon => Value::Photon(false),
            LuxType::Lumens => Value::Lumens(String::new()),
            LuxType::Umbra => Value::Umbra,

            // ─ composite types ────────────────────────────
            LuxType::Array(_) => Value::Array(Rc::new(RefCell::new(Vec::new()))),

            LuxType::Map(_, _) => Value::Map(Rc::new(RefCell::new(HashMap::new()))),

            // ─ named / user-defined types ─────────────────
            LuxType::Named(name) => match self.type_defs.get(name) {
                // type alias -> delegate to aliased type
                Some(TypeDef::Alias(aliased)) => self.get_zero_value(aliased),

                // facets behave like Go structs: default is nil / Umbra
                Some(TypeDef::Facet { .. }) => Value::Umbra,

                // unresolved -> warn, fall back to Umbra
                None => {
                    eprintln!(
                        "Warning: could not resolve type '{}', defaulting to Umbra",
                        name
                    );
                    Value::Umbra
                }
            },

            // catch-all for future variants
            _ => {
                eprintln!("Warning: unhandled type '{:?}', defaulting to Umbra", ty);
                Value::Umbra
            }
        }
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
    scopes: Vec<HashMap<String, (usize, Expr)>>,
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

    pub fn declare_local(&mut self, name: &str, expr: Expr) -> usize {
        let index = self.next_slot;
        self.next_slot += 1;

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), (index, expr));

        index
    }

    pub fn resolve_local(&self, name: &str) -> Option<(usize, Expr)> {
        for scope in self.scopes.iter().rev() {
            if let Some(&(index, ref expr)) = scope.get(name) {
                return Some((index, expr.clone()));
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
