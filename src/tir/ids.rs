/// Strongly-typed indices used throughout the TIR.
///
/// We keep them as `usize` wrappers so they’re zero-cost but still
/// different types in the type system.
macro_rules! new_id {
    ($name:ident) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub struct $name(pub usize);

        impl From<usize> for $name {
            fn from(u: usize) -> Self {
                Self(u)
            }
        }
        impl From<$name> for usize {
            fn from(id: $name) -> Self {
                id.0
            }
        }
    };
}

new_id!(ExprId); // expressions & literals
new_id!(StmtId); // statements / blocks
new_id!(LocalId); // compiler-time locals (not VM slots)
