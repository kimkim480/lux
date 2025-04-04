use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Light(f64),
    Lumens(String),
    Photon(bool),
    Umbra,
    Function(Rc<Function>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Light(n) => write!(f, "{}", n),
            Value::Lumens(s) => write!(f, "\"{}\"", s),
            Value::Photon(b) => write!(f, "{}", b),
            Value::Umbra => write!(f, "Umbra"),
            Value::Function(func) => write!(f, "{}", func),
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
    Print,
    GetGlobal(String),
    SetGlobal(String),
    Call(String),
    Return,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== Chunk: {name} ==");
        for (i, op) in self.code.iter().enumerate() {
            print!("{:04} ", i);
            match op {
                Op::Constant(index) => {
                    let value = &self.constants[*index];
                    println!("{:<16} {:>4} '{}'", "Constant", index, value);
                }
                Op::GetGlobal(name) => println!("{:<21} '{}'", "GetGlobal", name),
                Op::SetGlobal(name) => println!("{:<21} '{}'", "SetGlobal", name),
                Op::Call(name) => println!("{:<21} '{}'", "Call", name),
                other => println!("{:?}", other),
            }
        }
    }
}

#[derive(Debug)]
pub struct CallFrame {
    pub function: Rc<Function>,
    pub ip: usize,
    pub offset: usize, // start position on VM stack for this frame
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}
