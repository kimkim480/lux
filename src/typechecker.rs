use crate::ast::{Expr, Span, Spanned, Stmt, Type};
use crate::error::PrismError;
use crate::get_source_line;
use crate::value::TypeDef;
use core::fmt;
use std::collections::HashMap;

// Type representation for checking
#[derive(Clone, Debug, PartialEq)]
pub enum LuxType {
    Light,
    Photon,
    Lumens,
    Umbra,
    Array(Box<LuxType>),
    Named(String),
    Facet(String, Vec<(String, LuxType)>),
    Function(Vec<LuxType>, Box<LuxType>),
}

impl fmt::Display for LuxType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LuxType::Light => write!(f, "Light"),
            LuxType::Photon => write!(f, "Photon"),
            LuxType::Lumens => write!(f, "Lumens"),
            LuxType::Umbra => write!(f, "Umbra"),
            LuxType::Array(inner) => write!(f, "Array<{}>", inner),
            LuxType::Named(name) => write!(f, "{}", name),
            LuxType::Facet(name, _) => write!(f, "{}", name),
            LuxType::Function(_, return_type) => write!(f, "Function<{}>", return_type),
        }
    }
}

#[derive(Clone)]
pub struct TypeChecker {
    globals: HashMap<String, LuxType>,
    locals: Vec<HashMap<String, LuxType>>,
    pub type_defs: HashMap<String, TypeDef>,
    source: String,
    expected_return: Option<LuxType>,
}

impl TypeChecker {
    pub fn new(source: &String) -> Self {
        Self {
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
            type_defs: HashMap::new(),
            source: source.clone(),
            expected_return: None,
        }
    }

    pub fn check(&mut self, stmts: &[Spanned<Stmt>]) -> Result<(), PrismError> {
        // TODO: Maybe move this to a separate pass
        // Pass 1: collect type definitions (Refractions) and function signatures
        for stmt in stmts {
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
                            let t = self.resolve_type_hint(f_type)?;
                            Ok((f_name.clone(), t))
                        })
                        .collect::<Result<Vec<_>, PrismError>>()?;

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
                    self.globals.insert(name.clone(), fn_type);
                }
                _ => {}
            }
        }

        // Pass 2: full typecheck for all statements
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<(), PrismError> {
        match &stmt.node {
            Stmt::ConstDecl { name, ty, value } => {
                let expected = self.resolve_type_hint(ty)?;
                let actual = self.check_expr(value)?;
                if expected != actual {
                    return Err(PrismError::type_error(
                        format!(
                            "Type mismatch in const '{}': expected {}, got {}",
                            name, expected, actual
                        ),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
                self.define(name, expected);
            }

            Stmt::LetDecl { name, ty, value } => {
                let expected = self.resolve_type_hint(ty)?;
                let actual = self.check_expr(value)?;
                if expected != actual {
                    return Err(PrismError::type_error(
                        format!(
                            "Type mismatch in let '{}': expected {}, got {}",
                            name, expected, actual
                        ),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
                self.define(name, expected);
            }

            Stmt::FacetDecl { .. } => {}

            Stmt::FnDecl {
                name,
                params,
                body,
                return_type,
                ..
            } => {
                let fn_type = self.resolve_fn_signature(params, return_type)?;
                self.define(name, fn_type.clone());

                let prev = self.expected_return.clone();
                self.expected_return = Some(self.resolve_type_hint(return_type)?);

                self.enter_scope();

                for (param_name, param_type) in params {
                    let ty = self.resolve_type_hint(param_type)?;
                    self.define(param_name, ty);
                }

                for stmt in body {
                    self.check_stmt(&stmt)?;
                }

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

                let aliased_ty = self.resolve_type_hint(aliased)?;

                self.type_defs
                    .insert(name.clone(), TypeDef::Alias(aliased_ty));
            }

            Stmt::Return(expr_opt) => {
                let expected = self.expected_return.clone().unwrap_or(LuxType::Umbra);

                let actual = match expr_opt {
                    Some(expr) => self.check_expr(expr)?,
                    None => LuxType::Umbra,
                };

                if actual != expected {
                    return Err(PrismError::type_error(
                        format!(
                            "Return type mismatch: expected {}, got {}",
                            expected, actual
                        ),
                        &stmt.span,
                        &get_source_line(&self.source, stmt.span.line),
                    ));
                }
            }

            _ => {} // Skip switch, for, etc. for now
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Result<LuxType, PrismError> {
        // This will grow as we add more rules
        match &expr.node {
            Expr::Number(_) => Ok(LuxType::Light),
            Expr::String(_) => Ok(LuxType::Lumens),
            Expr::Bool(_) => Ok(LuxType::Photon),
            Expr::Umbra => Ok(LuxType::Umbra),
            Expr::Lambda {
                params,
                body,
                return_type,
                ..
            } => {
                self.enter_scope();

                let mut param_types = Vec::new();

                for (name, ty) in params {
                    let resolved = self.resolve_type_hint(ty)?;
                    self.define(name, resolved.clone());
                    param_types.push(resolved);
                }

                let prev = self.expected_return.clone();
                self.expected_return = Some(self.resolve_type_hint(return_type)?);

                for stmt in body {
                    self.check_stmt(stmt)?;
                }

                let return_ty = self.expected_return.clone().unwrap_or(LuxType::Umbra);

                self.expected_return = prev;
                self.exit_scope();

                Ok(LuxType::Function(param_types, Box::new(return_ty)))
            }

            Expr::Identifier(name) => self.lookup(&name, &expr.span),
            Expr::Assign { name, value } => {
                let expected = self.lookup(name, &expr.span)?;
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

                Ok(expected)
            }
            Expr::AssignOp { name, op, value } => {
                let lhs = self.lookup(name, &expr.span)?;
                let rhs = self.check_expr(value)?;

                if lhs != LuxType::Light || rhs != LuxType::Light {
                    return Err(PrismError::type_error(
                        format!("Operator '{}' only supports Light types", op),
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

                Ok(LuxType::Light)
            }
            Expr::AssignIndex {
                array,
                index,
                value,
            } => {
                let arr_ty = self.check_expr(array)?;
                let index_ty = self.check_expr(index)?;
                let value_ty = self.check_expr(value)?;

                let LuxType::Array(inner_ty) = arr_ty else {
                    return Err(PrismError::type_error(
                        format!("Cannot index into non-array type: {}", arr_ty),
                        &array.span,
                        &get_source_line(&self.source, array.span.line),
                    ));
                };

                if index_ty != LuxType::Light {
                    return Err(PrismError::type_error(
                        format!("Array index must be Light, got {}", index_ty),
                        &index.span,
                        &get_source_line(&self.source, index.span.line),
                    ));
                }

                if *inner_ty != value_ty {
                    return Err(PrismError::type_error(
                        format!(
                            "Array element type mismatch: expected {}, got {}",
                            inner_ty, value_ty
                        ),
                        &value.span,
                        &get_source_line(&self.source, value.span.line),
                    ));
                }

                Ok(inner_ty.as_ref().clone())
            }

            Expr::Array { elements } => {
                if elements.is_empty() {
                    return Err(PrismError::type_error(
                        "Cannot infer type of empty array — please add at least one item",
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    ));
                }

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

                Ok(LuxType::Array(Box::new(first_type)))
            }

            Expr::ArrayGet { array, index } => {
                let array_ty = self.check_expr(array)?;
                let index_ty = self.check_expr(index)?;

                // Ensure array is actually an array
                let LuxType::Array(inner) = array_ty else {
                    return Err(PrismError::type_error(
                        format!(
                            "Tried to index into value of type {}, which is not an array",
                            array_ty
                        ),
                        &array.span,
                        &get_source_line(&self.source, array.span.line),
                    ));
                };

                // Ensure index is a Light (number)
                if index_ty != LuxType::Light {
                    return Err(PrismError::type_error(
                        format!("Array index must be Light, got {}", index_ty),
                        &index.span,
                        &get_source_line(&self.source, index.span.line),
                    ));
                }

                Ok(*inner)
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

                        Ok(*return_type)
                    }

                    other => Err(PrismError::type_error(
                        format!("Cannot call value of type {}", other),
                        &callee.span,
                        &get_source_line(&self.source, callee.span.line),
                    )),
                }
            }

            Expr::Unary { op, expr } => {
                let expr_type = self.check_expr(expr)?;

                use crate::ast::UnaryOp::*;

                match op {
                    Bang => {
                        if expr_type == LuxType::Photon {
                            Ok(LuxType::Light)
                        } else {
                            Err(PrismError::type_error(
                                format!("Cannot apply {} to {}", op, expr_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ))
                        }
                    }
                    Minus => {
                        if expr_type == LuxType::Light {
                            Ok(LuxType::Light)
                        } else {
                            Err(PrismError::type_error(
                                format!("Cannot apply {} to {}", op, expr_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ))
                        }
                    }
                }
            }
            Expr::Binary { left, op, right } => {
                let left_type = self.check_expr(&left)?;
                let right_type = self.check_expr(&right)?;

                use crate::ast::BinaryOp::*;

                match op {
                    Plus | Minus | Star | Slash | Percent => {
                        if left_type == LuxType::Light && right_type == LuxType::Light {
                            Ok(LuxType::Light)
                        } else {
                            Err(PrismError::type_error(
                                format!("Cannot apply {} to {} and {}", op, left_type, right_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ))
                        }
                    }

                    EqualEqual | BangEqual => {
                        if left_type == right_type {
                            Ok(LuxType::Photon)
                        } else {
                            Err(PrismError::type_error(
                                format!(
                                    "Equality comparison requires same type: {} vs {}",
                                    left_type, right_type
                                ),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ))
                        }
                    }

                    Less | LessEqual | Greater | GreaterEqual => {
                        if left_type == LuxType::Light && right_type == LuxType::Light {
                            Ok(LuxType::Photon)
                        } else {
                            Err(PrismError::type_error(
                                format!("Cannot compare {} and {}", left_type, right_type),
                                &expr.span,
                                &get_source_line(&self.source, expr.span.line),
                            ))
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

                Ok(LuxType::Named(type_name.clone()))
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

                        Ok(field_type)
                    }

                    _ => Err(PrismError::type_error(
                        "Field access on non-facet type",
                        &expr.span,
                        &get_source_line(&self.source, expr.span.line),
                    )),
                }
            }

            _ => Ok(LuxType::Umbra), // placeholder fallback
        }
    }

    fn resolve_type_hint(&self, raw: &Type) -> Result<LuxType, PrismError> {
        match raw {
            Type::Light => Ok(LuxType::Light),
            Type::Lumens => Ok(LuxType::Lumens),
            Type::Photon => Ok(LuxType::Photon),
            Type::Umbra => Ok(LuxType::Umbra),
            Type::Array(inner) => Ok(LuxType::Array(Box::new(self.resolve_type_hint(inner)?))),
            Type::Named(name) => Ok(LuxType::Named(name.clone())),
            Type::Facet(name, fields) => {
                let resolved_fields = fields
                    .iter()
                    .map(|(name, ty)| {
                        let t = self.resolve_type_hint(ty)?;
                        Ok((name.clone(), t))
                    })
                    .collect::<Result<Vec<_>, PrismError>>()?;
                Ok(LuxType::Facet(name.clone(), resolved_fields))
            }
            Type::Function(params, return_type) => {
                let param_types = params
                    .iter()
                    .map(|ty| self.resolve_type_hint(ty))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(LuxType::Function(
                    param_types,
                    Box::new(self.resolve_type_hint(return_type)?),
                ))
            }
        }
    }

    fn resolve_fn_signature(
        &self,
        params: &[(String, Type)],
        return_type: &Type,
    ) -> Result<LuxType, PrismError> {
        let param_types = params
            .iter()
            .map(|(_, ty)| self.resolve_type_hint(ty))
            .collect::<Result<Vec<_>, _>>()?;

        let ret_ty = self.resolve_type_hint(return_type)?;
        Ok(LuxType::Function(param_types, Box::new(ret_ty)))
    }

    fn define(&mut self, name: &str, ty: LuxType) {
        self.locals.last_mut().unwrap().insert(name.to_string(), ty);
    }

    fn lookup(&self, name: &str, span: &Span) -> Result<LuxType, PrismError> {
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
}
