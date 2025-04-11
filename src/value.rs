use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Light(f64),
    Lumens(String),
    Photon(bool),
    Umbra,
    Function(Rc<RefCell<Function>>),
    Closure(Rc<Closure>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Light(n) => write!(f, "{}", n),
            Value::Lumens(s) => write!(f, "{}", s),
            Value::Photon(b) => write!(f, "{}", b),
            Value::Umbra => write!(f, "Umbra"),
            Value::Function(fun) => write!(f, "{}", fun.borrow()),
            Value::Closure(c) => write!(f, "{}", c.function.borrow()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Light(a), Value::Light(b)) => a == b,
            (Value::Lumens(a), Value::Lumens(b)) => a == b,
            (Value::Photon(a), Value::Photon(b)) => a == b,
            (Value::Umbra, Value::Umbra) => true,
            (Value::Function(_), Value::Function(_)) => false,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    Constant(usize),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Not,
    Negate,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Pop,
    Dup,
    Print,
    GetGlobal(String),
    SetGlobal(String),
    GetLocal(usize),
    SetLocal(usize),
    GetUpvalue(usize),
    SetUpvalue(usize),
    Closure {
        fn_index: usize,
        upvalues: Vec<(bool, usize)>, // (is_local, index)
    },
    Call(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Return,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn trace_op(&self, name: &str, ip: usize) {
        print!("{:<10} {:<04} ", name, ip);
        if let Some(op) = self.code.get(ip) {
            match op {
                Op::Add => println!("{:<21}", "Add"),
                Op::Sub => println!("{:<21}", "Sub"),
                Op::Mul => println!("{:<21}", "Mul"),
                Op::Div => println!("{:<21}", "Div"),
                Op::Rem => println!("{:<21}", "Rem"),
                Op::Not => println!("{:<21}", "Not"),
                Op::Negate => println!("{:<21}", "Negate"),
                Op::Equal => println!("{:<21}", "Equal"),
                Op::NotEqual => println!("{:<21}", "NotEqual"),
                Op::Less => println!("{:<21}", "Less"),
                Op::LessEqual => println!("{:<21}", "LessEqual"),
                Op::Greater => println!("{:<21}", "Greater"),
                Op::GreaterEqual => println!("{:<21}", "GreaterEqual"),
                Op::Print => println!("{:<21}", "Print"),

                Op::Constant(index) => {
                    let value = &self.constants[*index];
                    println!("{:<16} {:>6} '{}'", "Constant", index, value);
                }
                Op::GetGlobal(name) => println!("{:<21} '{}'", "GetGlobal", name),
                Op::SetGlobal(name) => println!("{:<21} '{}'", "SetGlobal", name),
                Op::GetLocal(slot) => {
                    println!("{:<21} {}", "GetLocal", slot)
                }
                Op::SetLocal(slot) => {
                    println!("{:<21} {}", "SetLocal", slot)
                }
                Op::GetUpvalue(index) => {
                    println!("{:<21} {}", "GetUpvalue", index)
                }
                Op::SetUpvalue(index) => {
                    println!("{:<21} {}", "SetUpvalue", index)
                }
                Op::Closure { fn_index, upvalues } => {
                    println!("{:<21} {}", "Closure", fn_index);
                    for (is_local, index) in upvalues {
                        println!(
                            "{:>15} {:<16} {:>6}",
                            "|",
                            if *is_local { "Local" } else { "Upvalue" },
                            index
                        );
                    }
                }
                Op::Pop => println!("{:<21}", "Pop"),
                Op::Dup => println!("{:<21}", "Dup"),
                Op::JumpIfFalse(offset) => {
                    println!("{:<21} {:04} -> {:04}", "JumpIfFalse", ip, offset)
                }
                Op::Jump(offset) => {
                    println!("{:<21} {:04} -> {:04}", "Jump", ip, offset)
                }
                Op::Call(arity) => println!("{:<21} {}", "Call", arity),
                Op::Return => println!("{:<21}", "Return"),
            }
        } else {
            println!("(invalid ip)");
        }
    }
}

#[derive(Debug)]
pub struct CallFrame {
    pub function: Rc<RefCell<Function>>,
    pub ip: usize,
    pub offset: usize, // start position on VM stack for this frame
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
    pub upvalue_count: usize,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}
#[derive(Debug)]
pub struct Closure {
    pub function: Rc<RefCell<Function>>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}
