use crate::compiler::{Chunk, Op};
use crate::value::Value;

pub type PrismResult<T> = Result<T, PrismError>;

#[derive(Debug)]
pub enum PrismError {
    Compile(String),
    Runtime(String),
}

pub struct Prism {
    ip: usize, // instruction pointer
    stack: Vec<Value>,
    chunk: Chunk,
}

impl Prism {
    pub fn new(chunk: Chunk) -> Self {
        Prism {
            ip: 0,
            stack: Vec::new(),
            chunk,
        }
    }

    fn debug_stack(&self) {
        if self.stack.is_empty() {
            println!("🟦 [stack] <empty>");
            return;
        }

        print!("🟦 [stack]");
        for val in &self.stack {
            print!(" | {}", val);
        }
        println!();
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

        self.stack.push(result.clone());
        Ok(result)
    }

    fn pop(&mut self) -> Result<Value, PrismError> {
        self.stack
            .pop()
            .ok_or_else(|| PrismError::Runtime("Stack underflow".into()))
    }

    pub fn run(&mut self) -> PrismResult<()> {
        use Op::*;

        while self.ip < self.chunk.code.len() {
            let op = &self.chunk.code[self.ip];
            self.ip += 1;
            // println!("🔹 [op] {:?}", op);

            match op {
                Const(index) => {
                    let value = self.chunk.constants[*index].clone();
                    self.stack.push(value);
                }
                Add | Sub | Mul | Div | Rem | Less | LessEqual | Greater | GreaterEqual => {
                    self.binary_op(op.clone())?;
                }
                Negate => {
                    let a = self.pop()?;
                    match a {
                        Value::Light(a) => self.stack.push(Value::Light(-a)),
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
                        Value::Photon(a) => self.stack.push(Value::Photon(!a)),
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
                    self.stack.push(Value::Photon(a == b));
                }
                NotEqual => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Photon(a != b));
                }
                Pop => {
                    self.pop()?;
                }
                Print => {
                    let value = self.pop()?;
                    println!("{}", value);
                }
                Return => {
                    return Ok(());
                }
            }

            // self.debug_stack();
        }

        Ok(())
    }
}
