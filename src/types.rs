pub type LocalVar = usize;
pub type Int = i64;
pub type Float = f64;
pub type Bool = bool;
pub type Label = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Any,
    Empty,
    Function { arguments: Vec<Box<Type>>, result: Box<Type> }
}
