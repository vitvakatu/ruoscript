use types::{Int, Float, Bool};

use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int(Int),
    Float(Float),
    Bool(Bool),
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (*self, *other) {
            (Value::Int(l), Value::Int(r)) => Some(l.cmp(&r)),
            (Value::Float(l), Value::Float(r)) => l.partial_cmp(&r),
            _ => None
        }
    }
}