use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    constants::{MAX_FRAMES, MAX_STACK},
    error::{PrismError, PrismResult},
    value::{CallFrame, Closure, Op, Upvalue, Value},
};

pub struct Prism {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
    pub debug_trace: bool,
}

impl Prism {
    pub fn new() -> Self {
        Prism {
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            debug_trace: false,
        }
    }

    fn binary_op(&mut self, op: Op) -> Result<Value, PrismError> {
        let b = self.pop()?;
        let a = self.pop()?;

        if self.debug_trace {
            // println!("a: {:?}\nb: {:?}", a, b);
        }

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

    pub fn capture_upvalue(&mut self, stack_index: usize) -> Rc<RefCell<Upvalue>> {
        // TODO: later, reuse same upvalue if already open
        Rc::new(RefCell::new(Upvalue::Open(stack_index)))
    }

    pub fn close_upvalues(&mut self, from_index: usize) {
        for frame in &mut self.frames {
            for up in &mut frame.upvalues {
                let mut u = up.borrow_mut();
                if let Upvalue::Open(i) = *u {
                    if i >= from_index {
                        println!("🔐 Closing upvalue at stack[{}] = {:?}", i, self.stack[i]);
                        let value = self.stack[i].clone();
                        *u = Upvalue::Closed(value);
                    }
                }
            }
        }
    }

    pub fn run(&mut self) -> PrismResult<()> {
        loop {
            let frame = self.frames.last_mut().unwrap();
            if frame.ip >= frame.function.borrow().chunk.code.len() {
                break;
            }

            if self.debug_trace {
                frame
                    .function
                    .borrow()
                    .chunk
                    .trace_op(&format!("[{}]", frame.function.borrow().name), frame.ip);
            }

            let op = {
                let op = frame.function.borrow().chunk.code[frame.ip].clone();
                frame.ip += 1;
                op
            };

            match op {
                Op::Constant(index) => {
                    let frame = self.frames.last().unwrap();
                    let value = frame.function.borrow().chunk.constants[index].clone();
                    self.push(value)?;
                }
                Op::Add
                | Op::Sub
                | Op::Mul
                | Op::Div
                | Op::Rem
                | Op::Less
                | Op::LessEqual
                | Op::Greater
                | Op::GreaterEqual => {
                    self.binary_op(op)?;
                }
                Op::Negate => {
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
                Op::Not => {
                    let a = self.pop()?;
                    match a {
                        Value::Photon(a) => self.push(Value::Photon(!a))?,
                        _ => {
                            return Err(PrismError::Runtime(
                                "Cannot apply '!' to non-Photon value".to_string(),
                            ));
                        }
                    }
                }
                Op::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Photon(a == b))?;
                }
                Op::NotEqual => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Photon(a != b))?;
                }
                Op::Pop => {
                    self.pop()?;
                }
                Op::Dup => {
                    let top = self
                        .stack
                        .last()
                        .cloned()
                        .ok_or_else(|| PrismError::Runtime("Nothing to duplicate".into()))?;
                    self.push(top)?;
                }
                Op::Print => {
                    let value = self.pop()?;
                    println!("{}", value);
                }
                Op::GetGlobal(name) => {
                    let value = self.globals.get(&name).cloned().ok_or_else(|| {
                        PrismError::Runtime(format!("Undefined global constant '{}'", name))
                    })?;
                    self.push(value)?;
                }
                Op::SetGlobal(name) => {
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
                Op::GetLocal(slot) => {
                    let index = frame.offset + slot;
                    let value = self.stack[index].clone();

                    self.push(value)?;
                }
                Op::SetLocal(slot) => {
                    let val = self.stack.pop().ok_or_else(|| {
                        PrismError::Runtime(format!(
                            "Value missing when assigning to local '{}'",
                            slot
                        ))
                    })?;

                    let index = frame.offset + slot;

                    if self.stack.len() <= index {
                        self.stack.resize(index + 1, Value::Umbra);
                    }

                    self.stack[index] = val;
                }
                Op::GetUpvalue(index) => {
                    let frame = self.frames.last().unwrap();

                    let value = frame.upvalues[index].borrow().clone();
                    match value {
                        Upvalue::Open(stack_index) => {
                            let val = self.stack[stack_index].clone();
                            self.push(val)?;
                        }
                        Upvalue::Closed(val) => {
                            self.push(val)?;
                        }
                    }
                }
                Op::SetUpvalue(index) => {
                    let val = self.pop()?;
                    let frame = self.frames.last().unwrap();
                    let mut upval = frame.upvalues[index].borrow_mut();
                    match *upval {
                        Upvalue::Open(stack_index) => {
                            self.stack[stack_index] = val.clone();
                        }
                        Upvalue::Closed(_) => {
                            *upval = Upvalue::Closed(val.clone());
                        }
                    }
                }
                Op::Closure { fn_index, upvalues } => {
                    let value = frame.function.borrow().chunk.constants[fn_index].clone();
                    let Value::Function(function) = value else {
                        return Err(PrismError::Runtime(format!(
                            "Expected function at constant {}, found {:?}",
                            fn_index, value
                        )));
                    };

                    let mut captured = Vec::new();
                    let offset = frame.offset;
                    for (is_local, index) in upvalues {
                        let upvalue = if is_local {
                            self.capture_upvalue(offset + index)
                        } else {
                            self.frames.last().unwrap().upvalues[index].clone()
                        };

                        captured.push(upvalue);
                    }

                    let closure = Closure {
                        function,
                        upvalues: captured,
                    };
                    self.push(Value::Closure(Rc::new(closure)))?;
                }
                Op::Call(arity) => {
                    if self.frames.len() >= MAX_FRAMES {
                        return Err(PrismError::Runtime(
                            "Stack overflow: too many nested calls.".to_string(),
                        ));
                    }

                    if self.stack.len() < arity + 1 {
                        return Err(PrismError::Runtime(format!(
                            "Not enough values on the stack for function call: need {} args + callee, got {}",
                            arity,
                            self.stack.len()
                        )));
                    }

                    let callee_index = self.stack.len() - 1;
                    let offset = self.stack.len() - arity - 1;
                    let callee = self.stack[callee_index].clone();

                    let closure = match callee {
                        Value::Closure(c) => c,
                        Value::Function(f) => Rc::new(Closure {
                            function: f.clone(),
                            upvalues: vec![],
                        }),
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to call a non-function.".to_string(),
                            ));
                        }
                    };

                    let frame = CallFrame {
                        function: closure.function.clone(),
                        ip: 0,
                        offset, // args are below callee
                        upvalues: closure.upvalues.clone(),
                    };

                    self.frames.push(frame);
                }
                Op::JumpIfFalse(offset) => {
                    let value = self.stack.last().unwrap();
                    match value {
                        Value::Photon(false) => frame.ip = offset,
                        Value::Photon(true) => {}
                        _ => {
                            return Err(PrismError::Runtime(
                                "Logical operation requires Photon value".to_string(),
                            ));
                        }
                    }
                }
                Op::Jump(offset) => {
                    frame.ip = offset;
                }
                Op::MakeArray(n) => {
                    if self.stack.len() < n {
                        return Err(PrismError::Runtime(
                            "Not enough values to form array".into(),
                        ));
                    }

                    let start = self.stack.len() - n;
                    let items = self.stack.drain(start..).collect::<Vec<_>>();
                    self.push(Value::Array(Rc::new(RefCell::new(items))))?;
                }
                Op::ArrayGet => {
                    let index_val = self.pop()?;
                    let array_val = self.pop()?;

                    let index = match index_val {
                        Value::Light(n) if n.fract() == 0.0 => n as usize,
                        _ => return Err(PrismError::Runtime("Invalid array index".into())),
                    };

                    match array_val {
                        Value::Array(arr) => {
                            if index >= arr.borrow().len() {
                                return Err(PrismError::Runtime(
                                    "Array index out of bounds".into(),
                                ));
                            }
                            self.push(arr.borrow()[index].clone())?;
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to index non-array value".into(),
                            ));
                        }
                    }
                }
                Op::ArraySet => {
                    let value = self.pop()?;
                    let index_val = self.pop()?;
                    let array_val = self.pop()?;

                    println!("value: {:?}", value);
                    println!("index_val: {:?}", index_val);
                    println!("array_val: {:?}", array_val);

                    let index = match index_val {
                        Value::Light(n) if n.fract() == 0.0 => n as usize,
                        _ => return Err(PrismError::Runtime("Invalid array index".into())),
                    };

                    match array_val {
                        Value::Array(arr) => {
                            if index >= arr.borrow().len() {
                                return Err(PrismError::Runtime(
                                    "Array index out of bounds".into(),
                                ));
                            }

                            arr.borrow_mut()[index] = value.clone();
                            self.push(Value::Umbra)?; // set returns nothing
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to index non-array value".into(),
                            ));
                        }
                    }
                }

                Op::Return => {
                    let return_value = self.pop()?; // 1. get return value
                    self.frames.pop(); // 2. pop frame
                    self.close_upvalues(self.stack.len()); // 3. close captured vars

                    // 4. remove locals from stack
                    self.stack.truncate(self.stack.len());

                    // 5. push return value for caller
                    self.push(return_value)?;

                    // 6. if no more frames, we’re done
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                }
            }
        }

        Ok(())
    }

    fn print_stack_trace(&self) {
        for (i, val) in self.stack.iter().enumerate() {
            print!("[{:02}] {}", i, val);
            if i < self.stack.len() - 1 {
                print!(" | ");
            }
        }
        if self.stack.len() > 0 {
            print!("\n🔹 Total stack size: {}\n", self.stack.len());
        }
    }
}
