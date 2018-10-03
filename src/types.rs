use failure::Error;
use parser::ast::Expr;
use std::collections::HashMap;

pub type LocalVar = usize;
pub type Int = i64;
pub type Float = f64;
pub type Bool = bool;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    String,
    Any,
    Empty,
    Function {
        arguments: Vec<Box<Type>>,
        result: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    expr: Box<Expr>,
    ty: Type,
}

impl TypedExpr {
    pub fn new(expr: Box<Expr>) -> TypedExpr {
        TypedExpr {
            expr,
            ty: Type::Any,
        }
    }
}
