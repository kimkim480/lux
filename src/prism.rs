use crate::compiler::{Chunk, Op};
use crate::value::Value;

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

    fn binary_op(&mut self, op: Op) -> Option<Value> {
        let b = self.stack.pop()?;
        let a = self.stack.pop()?;

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
                _ => return None,
            },
            _ => return None,
        };

        self.stack.push(result.clone());
        Some(result)
    }

    pub fn run(&mut self) -> Option<Value> {
        use Op::*;

        while self.ip < self.chunk.code.len() {
            let op = &self.chunk.code[self.ip];
            self.ip += 1;
            println!("🔹 [op] {:?}", op);

            match op {
                Const(index) => {
                    let value = self.chunk.constants[*index].clone();
                    self.stack.push(value);
                }
                Add | Sub | Mul | Div | Rem | Less | LessEqual | Greater | GreaterEqual => {
                    self.binary_op(op.clone())?;
                }
                Return => {
                    return self.stack.pop();
                }
                Negate => {
                    let a = self.stack.pop()?;
                    match a {
                        Value::Light(a) => self.stack.push(Value::Light(-a)),
                        _ => {
                            eprintln!("Runtime error: unsupported types for Negate");
                            return None;
                        }
                    }
                }
                Not => {
                    let a = self.stack.pop()?;
                    match a {
                        Value::Photon(a) => self.stack.push(Value::Photon(!a)),
                        _ => {
                            eprintln!("Runtime error: unsupported types for Not");
                            return None;
                        }
                    }
                }
                Equal => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    self.stack.push(Value::Photon(a == b));
                }
                NotEqual => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    self.stack.push(Value::Photon(a != b));
                }
            }

            self.debug_stack();
        }

        None
    }
}
