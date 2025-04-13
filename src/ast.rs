#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Light,
    Lumens,
    Umbra,
    Photon,
    Lambda,
    Array(Box<Type>),
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
pub enum LogicalOp {
    OrOr,   // ||
    AndAnd, // &&
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Bang,  // !
    Minus, // -
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // ||
    And,        // &&
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! -
    Call,       // function calls
}

impl Precedence {
    pub fn next(self) -> Precedence {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Call, // or Highest
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    String(String),
    Identifier(String),
    Umbra,
    Bool(bool),
    Assign {
        name: String,
        value: Box<Expr>,
    },
    AssignOp {
        name: String,
        op: BinaryOp,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: LogicalOp,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        params: Vec<(String, Type)>,
        body: Vec<Stmt>,
        arity: usize,
        return_type: Type,
    },
    Array {
        elements: Vec<Expr>,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    AssignIndex {
        array: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    FacetInit {
        type_name: String,
        fields: Vec<(String, Expr)>,
    },
    FieldGet {
        object: Box<Expr>,
        field: String,
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
    FacetDecl {
        name: String,
        fields: Vec<(String, Type)>,
    },
    TypeAlias {
        name: String,
        aliased: Type,
    },
    If {
        condition: Expr,
        body: Vec<Stmt>,
        else_body: Option<Vec<Stmt>>,
    },
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        post: Option<Expr>,
        body: Vec<Stmt>,
    },
    Switch {
        target: Expr,
        cases: Vec<(Expr, Vec<Stmt>)>,
        default: Option<Vec<Stmt>>,
    },
    Return(Option<Expr>),
    ConstellationDecl(String),
    Expr(Expr),
    Emit(Expr),
    Break,
    Continue,
}
