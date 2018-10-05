use failure::Error;
use parser::ast::Expr;
use std::collections::HashMap;

pub type LocalVar = usize;
pub type Int = i64;
pub type Float = f64;
pub type Bool = bool;

pub const TYPE_INTEGER: &str = "Int";
pub const TYPE_STRING: &str = "Str";
pub const TYPE_ANY: &str = "Any";
pub const TYPE_VOID: &str = "Void";

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub args: Vec<String>,
    pub ret: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    pub expr: Box<Expr>,
    pub ty: String,
}

impl TypedExpr {
    pub fn any(expr: Box<Expr>) -> Self {
        Self { expr, ty: TYPE_ANY.to_string() }
    }

    pub fn with_type(expr: Box<Expr>, ty: String) -> Self {
        Self { expr, ty }
    }
}
