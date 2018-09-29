type Ident = String;

/// Function prototype
#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(u32),
    Variable(String),
    /// Function call (function name, arguments)
    Call(String, Vec<Box<Expr>>),
    /// Function prototype (function name, arguments)
    Prototype(Prototype),
    /// Function itself (prototype + body)
    Function(Prototype, Box<Expr>),
}

pub mod helpers {
    use super::*;

    pub fn int(v: u32) -> Box<Expr> {
        Box::new(Expr::Integer(v))
    }

    pub fn variable<S: Into<String>>(v: S) -> Box<Expr> {
        Box::new(Expr::Variable(v.into()))
    }

    pub fn call<S: Into<String>>(ident: S, args: Vec<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::Call(ident.into(), args))
    }

    pub fn prototype<S: Into<String>>(ident: S, args: Vec<String>) -> Box<Expr> {
        Box::new(Expr::Prototype(Prototype {
            name: ident.into(),
            args,
        }))
    }

    pub fn function(proto: Prototype, body: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Function(proto, body))
    }

    pub fn add(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        call("+", vec![l, r])
    }

    pub fn sub(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        call("-", vec![l, r])
    }

    pub fn mul(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        call("*", vec![l, r])
    }

    pub fn div(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        call("/", vec![l, r])
    }

    pub fn pow(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        call("^", vec![l, r])
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmpOp {
    Lt,
    Le,
    Gt,
    Ge,
    NotEq,
    Eq,
}
