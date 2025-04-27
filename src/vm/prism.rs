use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    bytecode::Op,
    codegen::MethodInfo,
    constants::{MAX_FRAMES, MAX_STACK},
    error::{PrismError, PrismResult},
};

use super::{CallFrame, Closure, Upvalue, Value, value::MapKey};

#[derive(Clone)]
pub struct Prism {
    pub frames: Vec<CallFrame>,
    pub stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
    pub debug_trace: bool,
    pub facet_layouts: HashMap<String, Vec<String>>,
    pub refraction_methods: HashMap<(String, String), MethodInfo>,
}

impl Prism {
    pub fn new() -> Self {
        Prism {
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            debug_trace: false,
            facet_layouts: HashMap::new(),
            refraction_methods: HashMap::new(),
        }
    }

    fn binary_op(&mut self, op: Op) -> Result<Value, PrismError> {
        let b = self.pop()?;
        let a = self.pop()?;

        if self.debug_trace {
            // println!("a: {:?}\nop: {:?}\nb: {:?}", a, op, b);
        }

        let result = match (a, b) {
            (Value::Light(a), Value::Light(b)) => match op {
                Op::Add => Value::Light(a + b),
                Op::Sub => Value::Light(a - b),
                Op::Mul => Value::Light(a * b),
                Op::Div => Value::Light(a / b),
                Op::Rem => Value::Light(a % b),
                Op::Pow => Value::Light(a.powf(b)),
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
            let prism = self.clone();
            let frame = self.frames.last_mut().unwrap();
            if frame.ip >= frame.function.borrow().chunk.code.len() {
                break;
            }

            if self.debug_trace {
                prism.print_stack_trace();

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
                | Op::Pow
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

                Op::GetMethod(method_name) => {
                    let receiver = self.pop()?;

                    let facet_name = if let Value::Facet { type_name, .. } = &receiver {
                        type_name
                    } else {
                        return Err(PrismError::Runtime(format!(
                            "Tried to call method '{method_name}' on non-facet value"
                        )));
                    };

                    let method_info = self
                        .refraction_methods
                        .get(&(facet_name.clone(), method_name.clone()))
                        .cloned()
                        .ok_or_else(|| {
                            PrismError::Runtime(format!(
                                "Method '{facet_name}::{method_name}' not found"
                            ))
                        })?;

                    match (method_info.is_static, method_info.value) {
                        (true, method_val) => self.push(method_val)?,
                        (false, method_val) => {
                            self.push(receiver)?;
                            self.push(method_val)?;
                        }
                    }
                }

                Op::Invoke { method_name, arity } => {
                    println!("{:<21} {} {}", "Invoke", method_name, arity);
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

                Op::ArrayIndex => {
                    let index_val = self.pop()?;
                    let receiver_val = self.pop()?;

                    match receiver_val {
                        Value::Array(arr_rc) => {
                            let index = match index_val {
                                Value::Light(n) if n.fract() == 0.0 && n >= 0.0 => n as usize,
                                _ => {
                                    return Err(PrismError::Runtime(format!(
                                        "Array index must be a non-negative integer, but got {}",
                                        index_val
                                    )));
                                }
                            };

                            let arr_borrow = arr_rc.borrow();
                            if index >= arr_borrow.len() {
                                return Err(PrismError::Runtime(format!(
                                    "Array index out of bounds: index is {} but length is {}",
                                    index,
                                    arr_borrow.len()
                                )));
                            }
                            let element = arr_borrow[index].clone();
                            self.push(element)?;
                        }

                        _ => {
                            return Err(PrismError::Runtime(format!(
                                "Cannot apply index operation to {}",
                                receiver_val
                            )));
                        }
                    }
                }

                Op::ArraySlice => {
                    let index_val = self.pop()?;
                    let receiver_val = self.pop()?;

                    let Value::NumericRange { start, end } = index_val else {
                        return Err(PrismError::Runtime(format!(
                            "Slice operation requires a Range index, but got {}",
                            index_val
                        )));
                    };

                    if start.fract() != 0.0 || start < 0.0 || end.fract() != 0.0 || end < 0.0 {
                        return Err(PrismError::Runtime(format!(
                            "Invalid range bounds [{}..{}]. Must be non-negative integers.",
                            start, end
                        )));
                    }
                    let start_idx = start as usize;
                    let end_idx = end as usize; // Exclusive end

                    // Handle based on receiver type
                    match receiver_val {
                        Value::Array(arr_rc) => {
                            let arr_borrow = arr_rc.borrow();
                            let arr_len = arr_borrow.len();

                            // Clamp indices and create slice
                            let clamped_start = std::cmp::min(start_idx, arr_len);
                            let clamped_end = std::cmp::min(end_idx, arr_len);

                            let new_vec = if clamped_start <= clamped_end {
                                arr_borrow[clamped_start..clamped_end].to_vec()
                            } else {
                                Vec::new()
                            };
                            let new_array_value = Value::Array(Rc::new(RefCell::new(new_vec)));
                            self.push(new_array_value)?;
                        }

                        _ => {
                            return Err(PrismError::Runtime(format!(
                                "Cannot apply slice operation to {:?}",
                                receiver_val
                            )));
                        }
                    }
                }

                Op::ArraySet => {
                    let value = self.pop()?;
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

                Op::StringIndex => {
                    let index_val = self.pop()?;
                    let receiver_val = self.pop()?;

                    match receiver_val {
                        Value::Lumens(s) => {
                            let index = match index_val {
                                Value::Light(n) if n.fract() == 0.0 && n >= 0.0 => n as usize,
                                _ => {
                                    return Err(PrismError::Runtime(format!(
                                        "String index must be a non-negative integer, but got {}",
                                        index_val
                                    )));
                                }
                            };

                            // TODO: Decide string indexing semantics: bytes or chars?
                            // Byte access (simple, like Go)
                            // if index >= s.len() {
                            //     return Err(PrismError::Runtime(format!(
                            //         "String index out of bounds: index is {} but length is {}",
                            //         index, s.len()
                            //     )));
                            // }
                            // let byte_char = s.as_bytes()[index] as char; // This might not be a valid char
                            // self.push(Value::Lumens(byte_char.to_string()))?;

                            // This is safe (for UTF-8), though chars().count() can be O(n)
                            if let Some(ch) = s.chars().nth(index) {
                                self.push(Value::Lumens(ch.to_string()))?;
                            } else {
                                return Err(PrismError::Runtime(format!(
                                    "String index out of bounds: index is {} but char length is {}",
                                    index,
                                    s.chars().count()
                                )));
                            }
                        }

                        _ => {
                            return Err(PrismError::Runtime(format!(
                                "Cannot apply index operation to {}",
                                receiver_val
                            )));
                        }
                    }
                }

                Op::StringSlice => {
                    let index_val = self.pop()?;
                    let receiver_val = self.pop()?;

                    let Value::NumericRange { start, end } = index_val else {
                        return Err(PrismError::Runtime(format!(
                            "Slice operation requires a Range index, but got {}",
                            index_val
                        )));
                    };

                    if start.fract() != 0.0 || start < 0.0 || end.fract() != 0.0 || end < 0.0 {
                        return Err(PrismError::Runtime(format!(
                            "Invalid range bounds [{}..{}]. Must be non-negative integers.",
                            start, end
                        )));
                    }
                    let start_idx = start as usize;
                    let end_idx = end as usize; // Exclusive end

                    match receiver_val {
                        Value::Lumens(s) => {
                            // TODO: Decide string slicing semantics: bytes or chars?
                            // Byte slicing (simple, like Go)
                            // let byte_len = s.len();
                            // let clamped_start = std::cmp::min(start_idx, byte_len);
                            // let clamped_end = std::cmp::min(end_idx, byte_len);
                            // let slice_str = if clamped_start <= clamped_end {
                            //     &s[clamped_start..clamped_end]
                            // } else {
                            //     ""
                            // };
                            // self.push(Value::Lumens(slice_str.to_string()))?;

                            // This is safe (for UTF-8), though chars().count() can be O(n)
                            let char_count = s.chars().count();
                            let clamped_start = std::cmp::min(start_idx, char_count);
                            let clamped_end = std::cmp::min(end_idx, char_count);

                            let slice_str: String = if clamped_start <= clamped_end {
                                s.chars()
                                    .skip(clamped_start)
                                    .take(clamped_end - clamped_start)
                                    .collect()
                            } else {
                                String::new()
                            };
                            self.push(Value::Lumens(slice_str))?;
                        }

                        _ => {
                            return Err(PrismError::Runtime(format!(
                                "Cannot apply slice operation to {:?}",
                                receiver_val
                            )));
                        }
                    }
                }

                Op::Len => {
                    let array_val = self.pop()?;
                    match array_val {
                        Value::Array(arr) => self.push(Value::Light(arr.borrow().len() as f64))?,
                        Value::Lumens(str) => self.push(Value::Light(str.len() as f64))?,
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to get length of non-array value".into(),
                            ));
                        }
                    }
                }

                Op::Range => {
                    let end = self.pop()?;
                    let start = self.pop()?;

                    let start_num = match start {
                        Value::Light(n) => n,
                        _ => {
                            return Err(PrismError::Runtime(format!(
                                "Range start must be a number, but got '{}'.",
                                start
                            )));
                        }
                    };
                    let end_num = match end {
                        Value::Light(n) => n,
                        _ => {
                            return Err(PrismError::Runtime(format!(
                                "Range end must be a number, but got '{}'.",
                                end
                            )));
                        }
                    };

                    self.push(Value::NumericRange {
                        start: start_num,
                        end: end_num,
                    })?;
                }

                Op::ArrayPush => {
                    let array_val = self.pop()?;
                    let value = self.pop()?;

                    match array_val {
                        Value::Array(arr) => {
                            arr.borrow_mut().push(value);
                            self.push(Value::Umbra)?;
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to push to non-array value".into(),
                            ));
                        }
                    }
                }

                Op::ArrayPop => {
                    let array_val = self.pop()?;

                    match array_val {
                        Value::Array(arr) => {
                            if arr.borrow().is_empty() {
                                return Err(PrismError::Runtime(
                                    "Tried to pop from empty array".into(),
                                ));
                            }

                            let last = arr.borrow_mut().pop();
                            self.push(last.unwrap())?;
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to pop from non-array value".into(),
                            ));
                        }
                    }
                }
                Op::MakeFacet {
                    type_name,
                    field_count,
                } => {
                    if self.stack.len() < field_count {
                        return Err(PrismError::Runtime(format!(
                            "Not enough values to construct facet '{}'",
                            type_name
                        )));
                    }

                    let layout = self.facet_layouts.get(&type_name).ok_or_else(|| {
                        PrismError::Runtime(format!("Unknown facet type '{}'", type_name))
                    })?;

                    let values = self.stack.split_off(self.stack.len() - layout.len());

                    let mut fields = HashMap::new();
                    for (name, value) in layout.iter().zip(values) {
                        fields.insert(name.clone(), value);
                    }

                    let facet = Value::Facet {
                        type_name: type_name.clone(),
                        fields,
                    };

                    self.push(facet)?;
                }

                Op::FieldGet(name) => {
                    let value = self.pop()?;

                    match value {
                        Value::Facet { fields, .. } => {
                            let val = fields.get(&name).ok_or_else(|| {
                                PrismError::Runtime(format!("Field '{}' not found", name))
                            })?;
                            self.push(val.clone())?;
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to access field on non-facet value".into(),
                            ));
                        }
                    }
                }

                Op::MakeMap(pair_count) => {
                    if self.stack.len() < pair_count * 2 {
                        return Err(PrismError::Runtime(format!(
                            "Not enough values to construct map: need {} pairs, got {}",
                            pair_count,
                            self.stack.len()
                        )));
                    }

                    let mut map = HashMap::with_capacity(pair_count.next_power_of_two().max(1));
                    for _ in 0..pair_count {
                        let value = self.pop()?;
                        let key = self.pop()?;

                        let map_key = MapKey::try_from(key)?;

                        map.insert(map_key, value);
                    }

                    let map_value = Value::Map(Rc::new(RefCell::new(map)));

                    self.push(map_value)?;
                }

                Op::MapGet => {
                    let index_val = self.pop()?;
                    let receiver_val = self.pop()?;

                    match receiver_val {
                        Value::Map(map_rc) => {
                            let key = MapKey::try_from(index_val)?;
                            let map_borrow = map_rc.borrow();
                            let value = map_borrow.get(&key).ok_or_else(|| {
                                PrismError::Runtime(format!("Key '{}' not found in map", key))
                            })?;

                            self.push(value.clone())?;
                        }
                        _ => {
                            return Err(PrismError::Runtime(
                                "Tried to get key from non-map value".into(),
                            ));
                        }
                    }
                }

                Op::MapSet => {}

                Op::MapDelete => {}

                Op::Return => {
                    let frame = self.frames.pop().unwrap(); // 1. pop the call frame

                    self.close_upvalues(frame.offset); // 2. close captured vars

                    // 3. preserve return value from top of stack
                    let return_value = self.stack.pop().unwrap_or(Value::Umbra);

                    self.stack.truncate(frame.offset); // 4. remove locals + callee

                    self.push(return_value)?; // 5. push return for caller

                    if self.frames.is_empty() {
                        return Ok(()); // 6. exit if no frames
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
