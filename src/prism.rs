use std::collections::HashMap;

use crate::{
    constants::{MAX_FRAMES, MAX_STACK},
    value::{CallFrame, Chunk, Op, Value},
};

pub type PrismResult<T> = Result<T, PrismError>;

#[derive(Debug)]
pub enum PrismError {
    Compile(String),
    Runtime(String),
}

pub struct Prism {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
}

impl Prism {
    pub fn new() -> Self {
        Prism {
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    fn binary_op(&mut self, op: Op) -> Result<Value, PrismError> {
        let b = self.pop()?;
        let a = self.pop()?;

        let result = match (a, b) {
            (Value::Light(a), Value::Light(b)) => match op {
                Op::Add => Value::Light(a + b),
                Op::Sub => Value::Light(a - b),
                Op::Mul => Value::Light(a * b),
                Op::Div => Value::Light(a / b),
                Op::Rem => Value::Light(a % b),
                Op::Less => Value::Photon(a < b),
                Op::LessEqual => Value::Photon(a <= b),
                Op::Greater => Value::Photon(a > b),
                Op::GreaterEqual => Value::Photon(a >= b),
                _ => {
                    return Err(PrismError::Runtime(
                        "Unsupported types for binary operation".to_string(),
                    ));
                }
            },

            (Value::Lumens(a), Value::Lumens(b)) => match op {
                Op::Add => Value::Lumens(a + &b),
                _ => {
                    return Err(PrismError::Runtime(
                        "Unsupported types for binary operation".to_string(),
                    ));
                }
            },

            _ => {
                return Err(PrismError::Runtime(
                    "Unsupported types for binary operation".to_string(),
                ));
            }
        };

        self.push(result.clone())?;
        Ok(result)
    }

    fn push(&mut self, value: Value) -> Result<(), PrismError> {
        if self.stack.len() >= MAX_STACK {
            return Err(PrismError::Runtime("Stack overflow".to_string()));
        }
        self.stack.push(value);
        Ok(())
    }

    fn pop(&mut self) -> Result<Value, PrismError> {
        self.stack
            .pop()
            .ok_or_else(|| PrismError::Runtime("Stack underflow".into()))
    }

    pub fn run(&mut self) -> PrismResult<()> {
        use Op::*;

        loop {
            let op = {
                let frame = self.frames.last_mut().unwrap();
                if frame.ip >= frame.function.chunk.code.len() {
                    break;
                }
                let op = frame.function.chunk.code[frame.ip].clone();
                frame.ip += 1;
                op
            };

            match op {
                Constant(index) => {
                    let frame = self.frames.last().unwrap();
                    let value = frame.function.chunk.constants[index].clone();
                    self.push(value)?;
                }
                Add | Sub | Mul | Div | Rem | Less | LessEqual | Greater | GreaterEqual => {
                    self.binary_op(op)?;
                }
                Negate => {
                    let a = self.pop()?;
                    match a {
                        Value::Light(a) => self.push(Value::Light(-a))?,
                        _ => {
                            eprintln!("Runtime error: unsupported types for Negate");
                            return Err(PrismError::Runtime(
                                "Unsupported types for Negate".to_string(),
                            ));
                        }
                    }
                }
                Not => {
                    let a = self.pop()?;
                    match a {
                        Value::Photon(a) => self.push(Value::Photon(!a))?,
                        _ => {
                            eprintln!("Runtime error: unsupported types for Not");
                            return Err(PrismError::Runtime(
                                "Unsupported types for Not".to_string(),
                            ));
                        }
                    }
                }
                Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Photon(a == b))?;
                }
                NotEqual => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Photon(a != b))?;
                }
                Pop => {
                    self.pop()?;
                }
                Print => {
                    let value = self.pop()?;
                    println!("{}", value);
                }
                GetGlobal(name) => {
                    let value = self.globals.get(&name).cloned().ok_or_else(|| {
                        PrismError::Runtime(format!("Undefined global constant '{}'", name))
                    })?;
                    self.push(value)?;
                }
                SetGlobal(name) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        PrismError::Runtime(format!(
                            "Value missing when assigning to global constant '{}'",
                            name
                        ))
                    })?;

                    if self.globals.contains_key(&name) {
                        return Err(PrismError::Runtime(format!(
                            "Global constant '{}' already defined",
                            name
                        )));
                    }

                    self.globals.insert(name, value);
                }
                GetLocal(slot) => {
                    let val = self.stack.get(slot).cloned().ok_or_else(|| {
                        PrismError::Runtime(format!("undefined local variable '{}'", slot))
                    })?;
                    self.push(val)?;
                }
                SetLocal(slot) => {
                    let val = self.stack.pop().ok_or_else(|| {
                        PrismError::Runtime(format!(
                            "Value missing when assigning to local '{}'",
                            slot
                        ))
                    })?;
                    if self.stack.len() <= slot {
                        self.stack.resize(slot + 1, Value::Umbra);
                    }
                    self.stack[slot] = val;
                }
                Call(_) => {
                    if self.frames.len() >= MAX_FRAMES {
                        return Err(PrismError::Runtime(
                            "Stack overflow: too many nested calls".to_string(),
                        ));
                    }

                    let value = self
                        .stack
                        .pop()
                        .ok_or_else(|| PrismError::Runtime("Expected function".to_string()))?;

                    match value {
                        Value::Function(func) => {
                            let frame = CallFrame {
                                function: func.clone(),
                                ip: 0,
                                offset: self.stack.len(),
                            };
                            self.frames.push(frame);
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to call a non-function".to_string(),
                            ));
                        }
                    }
                }
                Return => {
                    self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(()); // program ends
                    }
                }
            }
        }

        Ok(())
    }
}
