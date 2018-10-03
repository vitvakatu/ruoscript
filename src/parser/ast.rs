/// Function prototype
#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub exprs: Vec<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i32),
    String(String),
    Variable(String),
    VariableDeclaration(String, Box<Expr>),
    /// Function call (function name, arguments)
    Call(String, Vec<Box<Expr>>),
    /// Function prototype (function name, arguments)
    Prototype(Prototype),
    /// Function itself (prototype + body)
    Function(Prototype, Block),
    Return(Box<Expr>),
}

pub mod helpers {
    #![allow(unused)]
    use super::*;

    pub fn int(v: i32) -> Box<Expr> {
        Box::new(Expr::Integer(v))
    }

    pub fn string<S: Into<String>>(s: S) -> Box<Expr> {
        Box::new(Expr::String(s.into()))
    }

    pub fn variable<S: Into<String>>(v: S) -> Box<Expr> {
        Box::new(Expr::Variable(v.into()))
    }

    pub fn var_declaration<S: Into<String>>(name: S, expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::VariableDeclaration(name.into(), expr))
    }

    pub fn ret(expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Return(expr))
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

    pub fn function(proto: Prototype, exprs: Vec<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::Function(proto, Block { exprs }))
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
