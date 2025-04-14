use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub filename: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Light,
    Lumens,
    Umbra,
    Photon,
    Array(Box<Type>),
    Named(String), // for user-defined types
    Facet(String, Vec<(String, Type)>),
    Function(Vec<Type>, Box<Type>),
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Star => write!(f, "*"),
            BinaryOp::Slash => write!(f, "/"),
            BinaryOp::Percent => write!(f, "%"),
            BinaryOp::EqualEqual => write!(f, "=="),
            BinaryOp::BangEqual => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "<="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOp {
    OrOr,   // ||
    AndAnd, // &&
}

impl fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogicalOp::OrOr => write!(f, "||"),
            LogicalOp::AndAnd => write!(f, "&&"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Bang,  // !
    Minus, // -
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Minus => write!(f, "-"),
        }
    }
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
        value: Box<Spanned<Expr>>,
    },
    AssignOp {
        name: String,
        op: BinaryOp,
        value: Box<Spanned<Expr>>,
    },
    Logical {
        left: Box<Spanned<Expr>>,
        op: LogicalOp,
        right: Box<Spanned<Expr>>,
    },
    Binary {
        left: Box<Spanned<Expr>>,
        op: BinaryOp,
        right: Box<Spanned<Expr>>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Spanned<Expr>>,
    },
    Call {
        callee: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    Lambda {
        params: Vec<(String, Type)>,
        body: Vec<Spanned<Stmt>>,
        arity: usize,
        return_type: Type,
    },
    Array {
        elements: Vec<Spanned<Expr>>,
    },
    ArrayGet {
        array: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },
    AssignIndex {
        array: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
        value: Box<Spanned<Expr>>,
    },
    FacetInit {
        type_name: String,
        fields: Vec<(String, Spanned<Expr>)>,
    },
    FieldGet {
        object: Box<Spanned<Expr>>,
        field: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ConstDecl {
        name: String,
        ty: Type,
        value: Spanned<Expr>,
    },
    FnDecl {
        name: String,
        params: Vec<(String, Type)>,
        arity: usize,
        body: Vec<Spanned<Stmt>>,
        return_type: Type,
    },
    LetDecl {
        name: String,
        ty: Type,
        value: Spanned<Expr>,
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
        condition: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>,
        else_body: Option<Vec<Spanned<Stmt>>>,
    },
    For {
        init: Option<Box<Spanned<Stmt>>>,
        condition: Option<Spanned<Expr>>,
        post: Option<Spanned<Expr>>,
        body: Vec<Spanned<Stmt>>,
    },
    Switch {
        target: Spanned<Expr>,
        cases: Vec<(Spanned<Expr>, Vec<Spanned<Stmt>>)>,
        default: Option<Vec<Spanned<Stmt>>>,
    },
    Return(Option<Spanned<Expr>>),
    ConstellationDecl(String),
    Expr(Spanned<Expr>),
    Emit(Spanned<Expr>),
    Break,
    Continue,
}
