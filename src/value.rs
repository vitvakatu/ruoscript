use types::{Bool, Float, Int, Type};

use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Empty,
    Int(Int),
    Float(Float),
    Bool(Bool),
    Function(fn(Value) -> ()),
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (&Value::Int(l), &Value::Int(r)) => Some(l.cmp(&r)),
            (&Value::Float(l), &Value::Float(r)) => l.partial_cmp(&r),
            _ => None,
        }
    }
}
