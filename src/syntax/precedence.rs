#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    None,
    Assignment, // =, +=, -=, *=, /=, %=
    Range,      // ..
    Or,         // ||
    And,        // &&
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // +, -
    Factor,     // *, /, %
    Unary,      // !, -
    Power,      // **
    Call,       // fn(), arr[index], obj.field, obj::method
}

impl Precedence {
    pub fn next(self) -> Precedence {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Range,
            Range => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Power,
            Power => Call,
            Call => Call, // or Highest
        }
    }
}
