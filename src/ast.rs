#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Light,
    Lumens,
    Umbra,
    Photon,
    Custom(String), // for user-defined types
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    EqualEqual,   // ==
    BangEqual,    // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Bang,  // !
    Minus, // -
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // + -
    Factor,     // * /
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    String(String),
    Identifier(String),
    Umbra,
    Bool(bool),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ConstDecl {
        name: String,
        ty: Type,
        value: Expr,
    },
    FnDecl {
        name: String,
        params: Vec<(String, Type)>,
        arity: usize,
        body: Vec<Stmt>,
        return_type: Type,
    },
    LetDecl {
        name: String,
        ty: Type,
        value: Expr,
    },
    ConstellationDecl(String),
    ExprStmt(Expr),
    EmitStmt(Expr),
}
