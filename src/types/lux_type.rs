use core::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct MethodInfo {
    pub ty: LuxType,
    pub is_static: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LuxType {
    // -------- Primitive types --------
    Light,  // numbers
    Photon, // booleans
    Lumens, // UTF-8 string
    Umbra,  // nil / unit

    // -------- Composite types --------
    Map(Box<LuxType>, Box<LuxType>),      // key, value
    Range,                                // half-open interval value (0..10)
    Array(Box<LuxType>),                  // homogeneous vector
    Named(String),                        // user-defined (Facet / Alias)
    Function(Vec<LuxType>, Box<LuxType>), // params, return
}

impl fmt::Display for LuxType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LuxType::Light => write!(f, "Light"),
            LuxType::Photon => write!(f, "Photon"),
            LuxType::Lumens => write!(f, "Lumens"),
            LuxType::Umbra => write!(f, "Umbra"),
            LuxType::Map(key, value) => {
                if key.as_ref() == &LuxType::Umbra && value.as_ref() == &LuxType::Umbra {
                    write!(f, "Map{{}}")
                } else {
                    write!(f, "Map<{}, {}>", key, value)
                }
            }
            LuxType::Range => write!(f, "Range"),
            LuxType::Array(inner) => write!(f, "Array<{}>", inner),
            LuxType::Named(name) => write!(f, "{}", name),
            LuxType::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Function({}) -> {}", params_str, ret)
            }
        }
    }
}
