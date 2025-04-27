use crate::error::PrismError;
use crate::get_source_line;
use crate::syntax::ast::{Module, Spanned};
use crate::syntax::{Expr, Span, Stmt};
use crate::types::LuxType;
use crate::types::lux_type::MethodInfo;
use crate::vm::TypeDef;
use std::collections::HashMap;

pub struct CheckedModule {
    pub ast: Module,
    // pub types: TypeTable,
}

#[derive(Clone)]
pub struct TypeChecker<'src> {
    globals: HashMap<String, LuxType>,
    locals: Vec<HashMap<String, LuxType>>,
    pub type_defs: HashMap<String, TypeDef>,
    pub refraction_methods: HashMap<(String, String), MethodInfo>,
    source: &'src String,
    expected_return: Option<LuxType>,
    did_return: bool,
    // pub types: TypeTable,
}

impl<'src> TypeChecker<'src> {
    pub fn new(source: &'src String) -> Self {
        Self {
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
            type_defs: HashMap::new(),
            refraction_methods: HashMap::new(),
            source,
            expected_return: None,
            did_return: false,
            // types: TypeTable::default(),
        }
    }

    pub fn check(&mut self, ast: Module) -> Result<CheckedModule, PrismError> {
        // TODO: Maybe move this to a separate pass
        // Pass 1: collect type definitions (Refractions) and function signatures
        for stmt in &ast.body {
            match &stmt.node {
                Stmt::FacetDecl { name, fields } => {
                    if self.type_defs.contains_key(name) {
                        return Err(PrismError::type_error(
                            format!("Type '{}' already declared", name),
                            &stmt.span,
                            &get_source_line(&self.source, stmt.span.line),
                        ));
                    }

                    let resolved_fields = fields
                        .iter()
                        .map(|(f_name, f_type)| {
                            let t = f_type.clone();
                            (f_name.clone(), t)
                        })
                        .collect::<Vec<_>>();

                    self.type_defs.insert(
                        name.clone(),
                        TypeDef::Facet {
                            fields: resolved_fields,
                        },
                    );
                }

                Stmt::FnDecl {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    let fn_type = self.resolve_fn_signature(params, return_type)?;
                    self.define_global(name, fn_type);
                }
                _ => {}
            }
        }

        // Pass 2: full typecheck for all statements
        for stmt in &ast.body {
            self.check_stmt(stmt)?;
        }

        Ok(CheckedModule {
            ast,
            // types: self.types,
        })
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<(), PrismError> {
        match &stmt.node {
            Stmt::ConstDecl { name, ty, value } => {
                let actual = self.check_expr(value)?;
                if *ty != actual {
                    return Err(PrismError::type_error(
                        format!(
                            "Type mismatch in const '{}': expected {}, got {}",
                            name, ty, actual
                        ),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
                self.define_local(name, ty.clone());
            }

            Stmt::LetDecl { name, ty, value } => {
                if self.lookup_local(name).is_some() {
                    return Err(PrismError::type_error(
                        format!("Cannot redefine variable '{}' in local scope", name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                if let LuxType::Map(key_ty, _) = &ty {
                    match key_ty.as_ref() {
                        LuxType::Light | LuxType::Photon | LuxType::Lumens => {} // OK
                        // TODO: Allow Named types? Only if they resolve to allowed types? Needs careful thought.
                        // LuxType::Named(_) => (), // Example: Allow named types for now
                        _ => {
                            return Err(PrismError::type_error(
                                format!(
                                    "Invalid map key type: {}. Map keys must be Light, Photon, or Lumens.",
                                    key_ty
                                ),
                                &stmt.span,
                                &get_source_line(&self.source, stmt.span.line),
                            ));
                        }
                    }
                }

                match value {
                    Some(expr) => {
                        let actual_type = self.check_expr(expr)?;

                        let types_match = match (&ty, &actual_type) {
                            (LuxType::Map(_, _), LuxType::Map(key, val))
                                // Map{} assigned to Map<K, V>
                                if key.as_ref() == &LuxType::Umbra
                                    && val.as_ref() == &LuxType::Umbra =>
                            {
                                true
                            }
                            (LuxType::Array(_), LuxType::Array(inner))
                                // [] assigned to Array<T>
                                if inner.as_ref() == &LuxType::Umbra =>
                            {
                                true
                            }
                            // Default case
                            (expected, actual) => *expected == actual,
                        };

                        if !types_match {
                            return Err(PrismError::type_error(
                                format!(
                                    "Type mismatch in let '{}': expected {}, but initializer has type {}",
                                    name, ty, actual_type
                                ),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }

                        self.define_local(name, ty.clone());
                    }
                    None => {
                        let supports_zero_value = match ty {
                            LuxType::Light
                            | LuxType::Photon
                            | LuxType::Lumens
                            | LuxType::Umbra
                            | LuxType::Array(_)
                            | LuxType::Map(_, _) => true,
                            // TODO: Add resolution for Named types
                            _ => false, // Range, Function, Facet likely don't support zero init
                        };

                        if !supports_zero_value {
                            return Err(PrismError::type_error(
                                format!(
                                    "Variable '{}' of type {} must be initialized (no default zero value)",
                                    name, ty
                                ),
                                &stmt.span,
                                &get_source_line(&self.source, stmt.span.line),
                            ));
                        }

                        self.define_local(name, ty.clone());
                    }
                };
            }

            Stmt::FacetDecl { .. } => {}
            Stmt::ConstellationDecl(_) => {}

            Stmt::RadiateDecl {
                facet_name,
                methods,
            } => {
                // for now just register the methods’ signatures in a facet‑method table.
                // We’ll enforce bodies / types in the next step.

                if let None = self.type_defs.get(facet_name) {
                    return Err(PrismError::type_error(
                        format!("Refraction type with name '{}' not found", facet_name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                for method in methods {
                    let sig = self.resolve_fn_signature(&method.params, &method.return_type)?;
                    let info = MethodInfo {
                        ty: sig,
                        is_static: method.is_static,
                    };

                    self.register_method(facet_name, &method.name, info, &stmt.span)?;
                }
            }

            Stmt::Emit(expr) => {
                self.check_expr(expr)?; // side effect only
            }

            Stmt::Break => {}
            Stmt::Continue => {}

            Stmt::If {
                condition,
                body,
                else_body,
            } => {
                let cond_ty = self.check_expr(&condition)?;
                if cond_ty != LuxType::Photon {
                    return Err(PrismError::type_error(
                        format!("Condition of 'if' must be Photon, got {}", cond_ty),
                        &condition.span,
                        &get_source_line(&self.source, condition.span.line),
                    ));
                }

                self.enter_scope();
                for stmt in body {
                    self.check_stmt(&stmt)?;
                }
                self.exit_scope();

                if let Some(else_body) = else_body {
                    self.enter_scope();
                    for stmt in else_body {
                        self.check_stmt(&stmt)?;
                    }
                    self.exit_scope();
                }
            }

            Stmt::Switch {
                target,
                cases,
                default,
            } => {
                let switch_ty = self.check_expr(&target)?;

                for (case_expr, case_body) in cases {
                    let case_ty = self.check_expr(&case_expr)?;
                    if case_ty != switch_ty {
                        return Err(PrismError::type_error(
                            format!(
                                "Switch case value type mismatch: expected {}, got {}",
                                switch_ty, case_ty
                            ),
                            &case_expr.span,
                            &get_source_line(&self.source, case_expr.span.line),
                        ));
                    }

                    self.enter_scope();
                    for stmt in case_body {
                        self.check_stmt(&stmt)?;
                    }
                    self.exit_scope();
                }

                if let Some(default_body) = default {
                    self.enter_scope();
                    for stmt in default_body {
                        self.check_stmt(&stmt)?;
                    }
                    self.exit_scope();
                }
            }

            Stmt::For {
                init,
                condition,
                post,
                body,
            } => {
                self.enter_scope();

                if let Some(init_stmt) = init {
                    self.check_stmt(&init_stmt)?;
                }

                if let Some(cond_expr) = condition {
                    let ty = self.check_expr(&cond_expr)?;
                    if ty != LuxType::Photon {
                        return Err(PrismError::type_error(
                            format!("Condition of 'for' must evaluate to Photon, got {}", ty),
                            &cond_expr.span,
                            &get_source_line(&self.source, cond_expr.span.line),
                        ));
                    }
                }

                if let Some(post_expr) = post {
                    self.check_expr(&post_expr)?; // side effect only
                }

                for stmt in body {
                    self.check_stmt(&stmt)?;
                }

                self.exit_scope();
            }

            Stmt::FnDecl {
                name,
                params,
                body,
                return_type,
                ..
            } => {
                if self.lookup_local(name).is_some() {
                    return Err(PrismError::type_error(
                        format!("Cannot redefine function '{}' in local scope", name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
                let fn_type = self.resolve_fn_signature(params, return_type)?;
                self.define_local(name, fn_type.clone());

                let prev = self.expected_return.clone();
                self.expected_return = Some(return_type.clone());

                self.enter_scope();

                for (param_name, param_type) in params {
                    let ty = param_type.clone();
                    self.define_local(param_name, ty);
                }

                // track whether a return actually happened
                let prev_return_flag = self.did_return;
                self.did_return = false;

                for stmt in body {
                    self.check_stmt(&stmt)?;
                }

                if !self.did_return && return_type != &LuxType::Umbra {
                    return Err(PrismError::type_error(
                        format!(
                            "Function '{}' declared to return {}, but has no return statement",
                            name, return_type
                        ),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                self.did_return = prev_return_flag;

                self.exit_scope();
                self.expected_return = prev;
            }

            Stmt::TypeAlias { name, aliased } => {
                if self.type_defs.contains_key(name) {
                    return Err(PrismError::type_error(
                        format!("Type '{}' already defined", name),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }

                let aliased_ty = aliased.clone();

                self.type_defs
                    .insert(name.clone(), TypeDef::Alias(aliased_ty));
            }

            Stmt::Expr(expr) => {
                self.check_expr(expr)?; // side effect only
            }

            Stmt::Return(expr_opt) => {
                self.did_return = true;
                let expected = self.expected_return.clone().unwrap_or(LuxType::Umbra);

                let resolved = match expected {
                    LuxType::Named(name) => {
                        let Some(TypeDef::Alias(alias)) = self.type_defs.get(&name) else {
                            return Err(PrismError::type_error(
                                format!("Type '{}' not found", name),
                                &stmt.span,
                                &get_source_line(&self.source, stmt.span.line),
                            ));
                        };

                        alias.clone()
                    }
                    _ => expected,
                };
                let actual = match expr_opt {
                    Some(expr) => self.check_expr(expr)?,
                    None => LuxType::Umbra,
                };

                if actual != resolved {
                    return Err(PrismError::type_error(
                        format!(
                            "Return type mismatch: expected {}, got {}",
                            resolved, actual
                        ),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Result<LuxType, PrismError> {
        // This will grow as we add more rules
        let expr_type = match &expr.node {
            Expr::Number(_) => LuxType::Light,
            Expr::String(_) => LuxType::Lumens,
            Expr::Identifier(name) => self.lookup_scopes(&name, &expr.span)?,
            Expr::Boolean(_) => LuxType::Photon,
            Expr::Umbra => LuxType::Umbra,
            Expr::Lambda {
                params,
                body,
                return_type,
                ..
            } => {
                self.enter_scope();

                let mut param_types = Vec::new();

                for (name, ty) in params {
                    let resolved = ty.clone();
                    self.define_local(name, resolved.clone());
                    param_types.push(resolved);
                }

                let prev = self.expected_return.clone();
                self.expected_return = Some(return_type.clone());

                for stmt in body {
                    self.check_stmt(stmt)?;
                }

                let return_ty = self.expected_return.clone().unwrap_or(LuxType::Umbra);

                self.expected_return = prev;
                self.exit_scope();

                LuxType::Function(param_types, Box::new(return_ty))
            }

            Expr::Assign { name, value } => {
                let expected = self.lookup_scopes(name, &expr.span)?;
                let actual = self.check_expr(value)?;

                if expected != actual {
                    return Err(PrismError::type_error(
                        format!(
                            "Cannot assign {} to '{}': expected {}",
                            actual, name, expected
                        ),
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

                expected
            }
            Expr::AssignOp { name, op, value } => {
                let lhs = self.lookup_scopes(name, &expr.span)?;
                let rhs = self.check_expr(value)?;

                if lhs != LuxType::Light || rhs != LuxType::Light {
                    return Err(PrismError::type_error(
                        format!("Operator '{}' only supports Light types", op),
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

                LuxType::Light
            }
            Expr::Logical { left, op, right } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;

                if left_ty != LuxType::Photon || right_ty != LuxType::Photon {
                    return Err(PrismError::type_error(
                        format!("Logical operator '{}' requires Photon types", op),
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

                LuxType::Photon
            }

            Expr::AssignIndex {
                callee,
                index,
                value,
            } => {
                let callee_ty = self.check_expr(callee)?;
                let index_ty = self.check_expr(index)?;
                let value_ty = self.check_expr(value)?;

                let (required_index_type, element_value_type) = match &callee_ty {
                    LuxType::Array(inner_type) => (LuxType::Light, *inner_type.clone()),
                    LuxType::Map(key_type, value_type) => (*key_type.clone(), *value_type.clone()),
                    _ => {
                        return Err(PrismError::type_error(
                            format!(
                                "Cannot assign via index to non-array/map type: {}",
                                callee_ty
                            ),
                            &callee.span,
                            &get_source_line(&self.source, callee.span.line),
                        ));
                    }
                };

                if element_value_type != value_ty {
                    return Err(PrismError::type_error(
                        format!(
                            "Type mismatch in assignment: cannot assign type '{}' to element/value of type '{}' in {}",
                            value_ty, element_value_type, callee_ty
                        ),
                        &value.span,
                        &get_source_line(&self.source, value.span.line),
                    ));
                }

                if index_ty != required_index_type {
                    return Err(PrismError::type_error(
                        format!(
                            "Type mismatch for index: {} requires index type '{}', but got '{}'",
                            callee_ty, required_index_type, index_ty
                        ),
                        &index.span,
                        &get_source_line(&self.source, index.span.line),
                    ));
                }

                value_ty
            }

            Expr::Array { elements } => {
                if elements.is_empty() {
                    LuxType::Array(Box::new(LuxType::Umbra))
                } else {
                    let first_type = self.check_expr(&elements[0])?;

                    for item in elements.iter().skip(1) {
                        let ty = self.check_expr(item)?;
                        if ty != first_type {
                            return Err(PrismError::type_error(
                                format!(
                                    "Array contains mixed types: expected {}, got {}",
                                    first_type, ty
                                ),
                                &item.span,
                                &get_source_line(&self.source, item.span.line),
                            ));
                        }
                    }

                    LuxType::Array(Box::new(first_type))
                }
            }

            Expr::Index { callee, index } => {
                let callee_ty = self.check_expr(callee)?;
                let index_ty = self.check_expr(index)?;

                let (required_index_type, result_type) = match &callee_ty {
                    LuxType::Array(inner_type) => (LuxType::Light, *inner_type.clone()),

                    // TODO: Add a type for characters
                    LuxType::Lumens => (LuxType::Light, LuxType::Lumens),

                    LuxType::Map(key_type, value_type) => (*key_type.clone(), *value_type.clone()),

                    _ => {
                        return Err(PrismError::type_error(
                            format!("Cannot index into non-indexable type: {}", callee_ty),
                            &callee.span,
                            &get_source_line(&self.source, callee.span.line),
                        ));
                    }
                };

                if index_ty != required_index_type {
                    return Err(PrismError::type_error(
                        format!(
                            "Type mismatch for index: expected {}, but got {}",
                            required_index_type, index_ty
                        ),
                        &index.span,
                        &get_source_line(&self.source, index.span.line),
                    ));
                }

                result_type
            }

            Expr::Slice { callee, range } => {
                let callee_ty = self.check_expr(callee)?;
                let range_ty = self.check_expr(range)?;

                if range_ty != LuxType::Range {
                    return Err(PrismError::type_error(
                        format!("Slice range must be Range, but got {}", range_ty),
                        &range.span,
                        &get_source_line(&self.source, range.span.line),
                    ));
                }

                match callee_ty {
                    LuxType::Array(inner_type) => *inner_type,
                    LuxType::Lumens => LuxType::Lumens,
                    _ => {
                        return Err(PrismError::type_error(
                            format!("Cannot slice non-array or non-string type: {}", callee_ty),
                            &callee.span,
                            &get_source_line(&self.source, callee.span.line),
                        ));
                    }
                }
            }

            Expr::Call { callee, args } => {
                let callee_type = self.check_expr(callee)?;
                let arg_types = args
                    .iter()
                    .map(|arg| self.check_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                match callee_type {
                    LuxType::Function(expected_params, return_type) => {
                        if expected_params.len() != arg_types.len() {
                            return Err(PrismError::type_error(
                                format!(
                                    "Function expects {} arguments, but got {}",
                                    expected_params.len(),
                                    arg_types.len()
                                ),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }

                        for (i, (expected, actual)) in
                            expected_params.iter().zip(&arg_types).enumerate()
                        {
                            if expected != actual {
                                return Err(PrismError::type_error(
                                    format!(
                                        "Argument {} type mismatch: expected {}, got {}",
                                        i + 1,
                                        expected,
                                        actual
                                    ),
                                    &args[i].span,
                                    &get_source_line(&self.source, args[i].span.line),
                                ));
                            }
                        }

                        *return_type
                    }

                    other => {
                        return Err(PrismError::type_error(
                            format!("Cannot call value of type {}", other),
                            &callee.span,
                            &get_source_line(&self.source, callee.span.line),
                        ));
                    }
                }
            }

            Expr::Unary { op, expr } => {
                let expr_type = self.check_expr(expr)?;

                use crate::syntax::ast::UnaryOp::*;

                match op {
                    Bang => {
                        if expr_type == LuxType::Photon {
                            LuxType::Light
                        } else {
                            return Err(PrismError::type_error(
                                format!("Cannot apply {} to {}", op, expr_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    }
                    Minus => {
                        if expr_type == LuxType::Light {
                            LuxType::Light
                        } else {
                            return Err(PrismError::type_error(
                                format!("Cannot apply {} to {}", op, expr_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    }
                }
            }
            Expr::Binary { left, op, right } => {
                let left_type = self.check_expr(&left)?;
                let right_type = self.check_expr(&right)?;

                use crate::syntax::ast::BinaryOp::*;

                match op {
                    Plus => {
                        if left_type == LuxType::Light && right_type == LuxType::Light {
                            LuxType::Light
                        } else if left_type == LuxType::Lumens && right_type == LuxType::Lumens {
                            LuxType::Lumens
                        } else {
                            return Err(PrismError::type_error(
                                format!("Cannot apply {} to {} and {}", op, left_type, right_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    }
                    Minus | Star | Slash | Percent | StarStar => {
                        if left_type == LuxType::Light && right_type == LuxType::Light {
                            LuxType::Light
                        } else {
                            return Err(PrismError::type_error(
                                format!("Cannot apply {} to {} and {}", op, left_type, right_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    }

                    EqualEqual | BangEqual => {
                        if left_type == right_type {
                            LuxType::Photon
                        } else {
                            return Err(PrismError::type_error(
                                format!(
                                    "Equality comparison requires same type: {} vs {}",
                                    left_type, right_type
                                ),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    }

                    Less | LessEqual | Greater | GreaterEqual => {
                        if left_type == LuxType::Light && right_type == LuxType::Light {
                            LuxType::Photon
                        } else {
                            return Err(PrismError::type_error(
                                format!("Cannot compare {} and {}", left_type, right_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        }
                    }
                }
            }

            Expr::FacetInit { type_name, fields } => {
                let declared = match self.type_defs.get(type_name) {
                    Some(TypeDef::Facet { fields }) => fields.clone(),
                    _ => {
                        return Err(PrismError::type_error(
                            format!("Type '{}' not found or not a Facet", type_name),
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    }
                };

                for (decl_name, decl_type) in declared {
                    let Some((_, value_expr)) = fields.iter().find(|(name, _)| *name == decl_name)
                    else {
                        return Err(PrismError::type_error(
                            format!("Missing field '{}' in '{}'", decl_name, type_name),
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    };

                    let actual_type = self.check_expr(value_expr)?;
                    let expected_type = decl_type.clone();

                    if actual_type != expected_type {
                        return Err(PrismError::type_error(
                            format!(
                                "Field '{}' in '{}' has wrong type: expected {}, got {}",
                                decl_name, type_name, expected_type, actual_type
                            ),
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    }
                }

                LuxType::Named(type_name.clone())
            }

            Expr::Map(key_value_pairs) => {
                if key_value_pairs.is_empty() {
                    // empty map type
                    LuxType::Map(Box::new(LuxType::Umbra), Box::new(LuxType::Umbra))
                } else {
                    let (first_key_expr, first_value_expr) = &key_value_pairs[0];
                    let first_key_type = self.check_expr(first_key_expr)?;
                    let first_value_type = self.check_expr(first_value_expr)?;

                    match &first_key_type {
                        LuxType::Light | LuxType::Photon | LuxType::Lumens => {} // OK
                        // TODO: Allow Named types? Only if they resolve to allowed types? Needs careful thought.
                        // LuxType::Named(_) => (), // Example: Allow named types for now
                        _ => {
                            return Err(PrismError::type_error(
                                format!(
                                    "Invalid map key type: {}. Map keys must be Light, Photon, or Lumens.",
                                    first_key_type
                                ),
                                &first_key_expr.span,
                                &get_source_line(&self.source, first_key_expr.span.line),
                            ));
                        }
                    }

                    for (key_expr, value_expr) in key_value_pairs.iter().skip(1) {
                        let current_key_type = self.check_expr(key_expr)?;
                        let current_value_type = self.check_expr(value_expr)?;

                        // Check key type consistency
                        if current_key_type != first_key_type {
                            return Err(PrismError::type_error(
                                format!(
                                    "Inconsistent map key types: expected {}, but found {}",
                                    first_key_type, current_key_type
                                ),
                                &key_expr.span,
                                &get_source_line(&self.source, key_expr.span.line),
                            ));
                        }

                        // Check value type consistency
                        if current_value_type != first_value_type {
                            return Err(PrismError::type_error(
                                format!(
                                    "Inconsistent map value types: expected {}, but found {}",
                                    first_value_type, current_value_type
                                ),
                                &value_expr.span,
                                &get_source_line(&self.source, value_expr.span.line),
                            ));
                        }
                    }

                    // All checks passed, return the inferred map type
                    LuxType::Map(Box::new(first_key_type), Box::new(first_value_type))
                }
            }

            Expr::FieldGet { object, field } => {
                let object_type = self.check_expr(&object)?;

                match object_type {
                    LuxType::Named(type_name) => {
                        let Some(TypeDef::Facet { fields }) = self.type_defs.get(&type_name) else {
                            return Err(PrismError::type_error(
                                format!("Type '{}' not found or not a Facet", type_name),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ));
                        };

                        let field_type = fields
                            .iter()
                            .find(|(f, _)| f == field)
                            .map(|(_, t)| t.clone())
                            .ok_or_else(|| {
                                PrismError::type_error(
                                    format!("Field '{}' not found on type '{}'", field, type_name),
                                    &expr.span,
                                    &get_source_line(&self.source, expr.span.line),
                                )
                            })?;

                        field_type
                    }

                    _ => {
                        return Err(PrismError::type_error(
                            "Field access on non-facet type",
                            &expr.span,
                            &get_source_line(&self.source, expr.span.line),
                        ));
                    }
                }
            }

            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let receiver_type =
                    match self.try_handle_static_call(&receiver, method, args, &receiver.span) {
                        Some(Ok(receiver_ty)) => {
                            // self.types.insert(receiver.id, receiver_ty.clone());
                            return Ok(receiver_ty);
                        }
                        Some(Err(e)) => return Err(e),
                        None => self.check_expr(&receiver)?,
                    };

                // --- Start: Handle Built-in Methods ---
                match &receiver_type {
                    LuxType::Array(inner_type) => {
                        match method.as_str() {
                            "len" => {
                                if !args.is_empty() {
                                    return Err(PrismError::type_error(
                                        "Method 'len' on Array takes no arguments".to_string(),
                                        &receiver.span,
                                        &get_source_line(&self.source, receiver.span.line),
                                    ));
                                }
                                // `len` returns a value of type Light
                                // self.types.insert(receiver.id, LuxType::Light);
                                return Ok(LuxType::Light);
                            }
                            "push" => {
                                if args.len() != 1 {
                                    return Err(PrismError::type_error(
                                        "Method 'push' on Array expects exactly one argument"
                                            .to_string(),
                                        &receiver.span,
                                        &get_source_line(&self.source, receiver.span.line),
                                    ));
                                }
                                let arg_expr = &args[0];
                                let arg_type = self.check_expr(arg_expr)?;
                                if arg_type != **inner_type {
                                    return Err(PrismError::type_error(
                                        format!(
                                            "Array::push() type mismatch: expected element type '{}', but got '{}'",
                                            inner_type, arg_type
                                        ),
                                        &arg_expr.span,
                                        &get_source_line(&self.source, arg_expr.span.line),
                                    ));
                                }
                                // `push` returns null/nil/void (Umbra)
                                // self.types.insert(receiver.id, LuxType::Umbra);
                                return Ok(LuxType::Umbra);
                            }
                            "pop" => {
                                if !args.is_empty() {
                                    return Err(PrismError::type_error(
                                        "Method 'pop' on Array takes no arguments".to_string(),
                                        &receiver.span,
                                        &get_source_line(&self.source, receiver.span.line),
                                    ));
                                }
                                // `pop` returns an element of the array's inner type. i.e [Light]::pop() -> Light
                                // self.types.insert(receiver.id, *inner_type.clone());
                                return Ok(*inner_type.clone());
                            }
                            _ => {} // Unknown built-in for Array: Do nothing, fall through to Facet check
                        }
                    }

                    LuxType::Lumens => {
                        match method.as_str() {
                            "len" => {
                                if !args.is_empty() {
                                    return Err(PrismError::type_error(
                                        "Method 'len' on Lumens takes no arguments".to_string(),
                                        &receiver.span,
                                        &get_source_line(&self.source, receiver.span.line),
                                    ));
                                }
                                // `len` returns a value of type Light
                                // self.types.insert(receiver.id, LuxType::Light);
                                return Ok(LuxType::Light);
                            }
                            _ => {} // Unknown built-in for Lumens: Do nothing, fall through to Facet check
                        }
                    }

                    _ => {} // Not a type with known built-in methods: Do nothing, fall through to Facet check
                }
                // --- End: Handle Built-in Methods ---

                // --- Start: Handle Facet (Refraction) Methods ---
                let receiver_name = match &receiver_type {
                    LuxType::Named(name) => {
                        // TODO: Later we will allow methods on aliased types
                        let Some(TypeDef::Facet { .. }) = self.type_defs.get(name) else {
                            return Err(PrismError::type_error(
                                format!(
                                    "Cannot call method '{}' on non-Facet type '{}'",
                                    method, name
                                ),
                                &receiver.span,
                                &get_source_line(&self.source, receiver.span.line),
                            ));
                        };

                        name
                    }
                    _ => {
                        return Err(PrismError::type_error(
                            format!("Method not found for type '{}'", receiver_type),
                            &receiver.span,
                            &get_source_line(&self.source, receiver.span.line),
                        ));
                    }
                };

                let key = (receiver_name.clone(), method.clone());
                let Some(info) = self.refraction_methods.get(&key).cloned() else {
                    return Err(PrismError::type_error(
                        format!("Method '{}' not found for type '{}'", method, receiver_name),
                        &receiver.span,
                        &get_source_line(&self.source, receiver.span.line),
                    ));
                };

                if info.is_static {
                    return Err(PrismError::type_error(
                        format!(
                            "Method '{}' is static and cannot be called on an instance of '{}'",
                            method, receiver_name
                        ),
                        &receiver.span,
                        &get_source_line(&self.source, receiver.span.line),
                    ));
                }

                let LuxType::Function(param_types, return_type) = &info.ty else {
                    // This is just a safeguard
                    return Err(PrismError::type_error(
                        format!(
                            "Method definition '{}' for '{}' is not a function type",
                            method, receiver_name
                        ),
                        &receiver.span,
                        &get_source_line(&self.source, receiver.span.line),
                    ));
                };

                // Validate argument count (remembering the implicit 'self' parameter)
                // Instance methods expect (self_param + explicit_args)
                if args.len() != param_types.len() - 1 {
                    return Err(PrismError::type_error(
                        format!(
                            "Method '{}' on type '{}' expects {} argument(s), but received {}",
                            method,
                            receiver_name,
                            param_types.len() - 1,
                            args.len()
                        ),
                        &receiver.span,
                        &get_source_line(&self.source, receiver.span.line),
                    ));
                }

                // Validate argument types (skipping the implicit 'self' type)
                for (arg, expected_type) in args.iter().zip(param_types.iter().skip(1)) {
                    let actual_type = self.check_expr(arg)?;
                    if actual_type != *expected_type {
                        return Err(PrismError::type_error(
                            format!(
                                "Argument type mismatch in call to method '{}' on '{}': expected '{}', but got '{}'",
                                method, receiver_name, expected_type, actual_type
                            ),
                            &arg.span,
                            &get_source_line(&self.source, arg.span.line),
                        ));
                    }
                }

                // If all checks pass for the Facet method, return its declared return type
                *return_type.clone()
                // --- End: Handle Facet (Refraction) Methods ---
            }
            Expr::Range { start, end } => {
                let start_type = self.check_expr(start)?;
                let end_type = self.check_expr(end)?;

                if start_type != LuxType::Light || end_type != LuxType::Light {
                    return Err(PrismError::type_error(
                        "Range start and end must be of type Light".to_string(),
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

                LuxType::Range
            }
        };

        // self.types.insert(expr.id, expr_type.clone());
        Ok(expr_type)
    }

    fn register_method(
        &mut self,
        facet_name: &String,
        method: &String,
        signature: MethodInfo,
        span: &Span,
    ) -> Result<(), PrismError> {
        if self
            .refraction_methods
            .contains_key(&(facet_name.clone(), method.clone()))
        {
            return Err(PrismError::type_error(
                format!("Method '{}' already declared", method),
                &span,
                &get_source_line(&self.source, span.line),
            ));
        }

        self.refraction_methods
            .insert((facet_name.clone(), method.clone()), signature);

        Ok(())
    }

    fn resolve_fn_signature(
        &self,
        params: &[(String, LuxType)],
        return_type: &LuxType,
    ) -> Result<LuxType, PrismError> {
        let param_types = params.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();

        let ret_ty = return_type.clone();
        Ok(LuxType::Function(param_types, Box::new(ret_ty)))
    }

    fn define_local(&mut self, name: &str, ty: LuxType) {
        self.locals.last_mut().unwrap().insert(name.to_string(), ty);
    }

    fn lookup_local(&self, name: &str) -> Option<&LuxType> {
        self.locals.last().unwrap().get(name)
    }

    fn define_global(&mut self, name: &str, ty: LuxType) {
        self.globals.insert(name.to_string(), ty);
    }

    fn lookup_scopes(&self, name: &str, span: &Span) -> Result<LuxType, PrismError> {
        for scope in self.locals.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Ok(ty.clone());
            }
        }
        if let Some(ty) = self.globals.get(name) {
            return Ok(ty.clone());
        }

        Err(PrismError::type_error(
            format!("Unknown identifier '{}'", name),
            span,
            &get_source_line(&self.source, span.line),
        ))
    }

    fn enter_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.locals.pop();
    }

    /// Attempts to type-check a method call as a static call (TypeName::method()).
    ///
    /// Returns:
    /// - `Some(Ok(return_type))` if it's a valid static call.
    /// - `Some(Err(error))` if it looks like a static call but has an error
    ///   (e.g., method not static, wrong args, method not found on type).
    /// - `None` if the `type_name` identifier doesn't represent a known Facet type,
    ///   indicating it should be treated as an instance call instead.
    fn try_handle_static_call(
        &mut self,
        receiver: &Box<Spanned<Expr>>,
        method: &String,
        args: &[Spanned<Expr>],
        receiver_span: &Span,
    ) -> Option<Result<LuxType, PrismError>> {
        let type_name = match &receiver.node {
            Expr::Identifier(name) => name.clone(),
            _ => "".to_string(),
        };

        if let Some(type_def) = self.type_defs.get(&type_name) {
            if let TypeDef::Facet { .. } = type_def {
                let key = (type_name.clone(), method.clone());

                if let Some(info) = self.refraction_methods.get(&key).cloned() {
                    if !info.is_static {
                        let err = PrismError::type_error(
                            format!(
                                "Cannot call instance method '{}' using static syntax '::' on type '{}'",
                                method, type_name
                            ),
                            receiver_span,
                            &get_source_line(&self.source, receiver_span.line),
                        );
                        return Some(Err(err));
                    }

                    let LuxType::Function(param_types, return_type) = &info.ty else {
                        let err = PrismError::type_error(
                            format!(
                                "Static method definition '{}::{}' is not stored as a function type.",
                                type_name, method
                            ),
                            receiver_span,
                            &get_source_line(&self.source, receiver_span.line),
                        );
                        return Some(Err(err));
                    };

                    if args.len() != param_types.len() {
                        let err = PrismError::type_error(
                            format!(
                                "Static method '{}::{}' expects {} argument(s), but received {}",
                                type_name,
                                method,
                                param_types.len(),
                                args.len()
                            ),
                            receiver_span,
                            &get_source_line(&self.source, receiver_span.line),
                        );
                        return Some(Err(err));
                    }

                    for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                        let actual_type = match self.check_expr(arg) {
                            Ok(t) => t,
                            Err(e) => return Some(Err(e)),
                        };
                        if actual_type != *expected_type {
                            let err = PrismError::type_error(
                                format!(
                                    "Argument type mismatch in static call to '{}::{}': parameter expected '{}', but got '{}'",
                                    type_name, method, expected_type, actual_type
                                ),
                                &arg.span,
                                &get_source_line(&self.source, arg.span.line),
                            );
                            return Some(Err(err));
                        }
                    }

                    return Some(Ok(*return_type.clone()));
                } else {
                    let err = PrismError::type_error(
                        format!(
                            "Static method or associated function '{}' not found for type '{}'",
                            method, type_name
                        ),
                        receiver_span,
                        &get_source_line(&self.source, receiver_span.line),
                    );
                    return Some(Err(err));
                }
            }
        }

        None
    }
}
