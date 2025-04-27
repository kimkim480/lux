use core::fmt;

use crate::{tir::ExprId, types::LuxType};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Span {
    pub filename: String,
    pub line: usize,
    pub column: usize,
    pub column_start: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub id: ExprId,
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(id: ExprId, node: T, span: Span) -> Self {
        Self { id, node, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,         // +
    Minus,        // -
    Star,         // *
    StarStar,     // **
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
            BinaryOp::StarStar => write!(f, "**"),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    String(String),
    Identifier(String),
    Umbra,
    Boolean(bool),
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
        params: Vec<(String, LuxType)>,
        body: Vec<Spanned<Stmt>>,
        arity: usize,
        return_type: LuxType,
    },
    Array {
        elements: Vec<Spanned<Expr>>,
    },
    Index {
        callee: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },
    Slice {
        callee: Box<Spanned<Expr>>,
        range: Box<Spanned<Expr>>,
    },
    AssignIndex {
        callee: Box<Spanned<Expr>>,
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
    MethodCall {
        receiver: Box<Spanned<Expr>>,
        method: String,
        args: Vec<Spanned<Expr>>,
    },
    Range {
        start: Box<Spanned<Expr>>,
        end: Box<Spanned<Expr>>,
    },
    Map(Vec<(Spanned<Expr>, Spanned<Expr>)>), // (key, value)
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodSig {
    pub name: String,
    pub params: Vec<(String, LuxType)>, // (param_name, type) i.e. fn(a: T) = (a, T)
    pub arity: usize,
    pub body: Vec<Spanned<Stmt>>,
    pub return_type: LuxType,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    RadiateDecl {
        facet_name: String,
        methods: Vec<MethodSig>,
    },
    ConstDecl {
        name: String,
        ty: LuxType,
        value: Spanned<Expr>,
    },
    FnDecl {
        name: String,
        params: Vec<(String, LuxType)>, // (param_name, type) i.e. fn(a: T) = (a, T)
        arity: usize,
        body: Vec<Spanned<Stmt>>,
        return_type: LuxType,
    },
    LetDecl {
        name: String,
        ty: LuxType,
        value: Option<Spanned<Expr>>,
    },
    FacetDecl {
        name: String,
        fields: Vec<(String, LuxType)>,
    },
    TypeAlias {
        name: String,
        aliased: LuxType,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub body: Vec<Spanned<Stmt>>,
}

impl Module {
    pub fn new(body: Vec<Spanned<Stmt>>) -> Self {
        Self { body }
    }
}
