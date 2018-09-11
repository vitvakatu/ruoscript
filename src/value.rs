use stack::Stack;
use types::{Bool, Float, Int};

use std::cmp::Ordering;
use std::fmt;

#[derive(Clone)]
pub enum Value {
    Empty,
    Int(Int),
    Float(Float),
    Bool(Bool),
    String(String),
    Function(fn(&mut Stack, u8)),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Value::Empty => write!(f, "()"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(float) => write!(f, "{}", float),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(ref s) => write!(f, "{}", s),
            Value::Function(_) => write!(f, "<Native function>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Empty, Value::Empty) => true,
            (Value::Int(ref l), Value::Int(ref r)) => l == r,
            (Value::Float(ref l), Value::Float(ref r)) => l == r,
            (Value::Bool(ref l), Value::Bool(ref r)) => l == r,
            (Value::String(ref l), Value::String(ref r)) => l == r,
            _ => false,
        }
    }
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
