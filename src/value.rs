#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Light(f64),
    Lumens(String),
    Photon(bool),
    Umbra,
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Light(n) => write!(f, "{}", n),
            Value::Lumens(s) => write!(f, "\"{}\"", s),
            Value::Photon(b) => write!(f, "{}", b),
            Value::Umbra => write!(f, "Umbra"),
            // later: Instance, Function, etc.
        }
    }
}
