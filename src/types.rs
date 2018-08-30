pub type LocalVar = usize;
pub type Int = i64;
pub type Float = f64;
pub type Bool = bool;
pub type Ident = String;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Label {
    Function(Ident, bool),
    Direct(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Any,
    Empty,
    Function {
        arguments: Vec<Box<Type>>,
        result: Box<Type>,
    },
}
