type Ident = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Empty,
    LParen,
    RParen,
    Ident(Ident),
    DeclareVar(Box<Expr>, Box<Expr>),
    Assign(Ident, Box<Expr>),
    FunCall(Ident, Vec<Box<Expr>>),
    Return(Box<Expr>),
    Block(Vec<Box<Expr>>),
    WhileLoop(Box<Expr>, Box<Expr>),
    FunDecl(Ident, Vec<Ident>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

pub trait Emit {
    fn emit(&self) -> String;
}

fn is_binop(func: &str) -> bool {
    match func {
        "+" => true,
        "-" => true,
        "*" => true,
        "/" => true,
        "^" => true,
        _ => false,
    }
}

impl Emit for Expr {
    fn emit(&self) -> String {
        let mut result = String::new();
        match *self {
            Expr::Int(i) => result.push_str(&i.to_string()),
            Expr::Return(ref expr) => {
                result.push_str("return ");
                result.push_str(&expr.emit());
                result.push_str(";\n");
            }
            Expr::DeclareVar(ref lvalue, ref expr) => {
                result.push_str("int ");
                result.push_str(&lvalue.emit());
                result.push_str(" = ");
                result.push_str(&expr.emit());
                result.push_str(";\n");
            }
            Expr::Ident(ref ident) => {
                result.push_str(&ident);
            }
            Expr::Block(ref exprs) => {
                for e in exprs {
                    result.push_str(&e.emit());
                }
            }
            Expr::FunCall(ref ident, ref args) => {
                if is_binop(&ident) {
                    assert_eq!(args.len(), 2);
                    let (l, r) = (&args[0], &args[1]);
                    result.push_str(&l.emit());
                    result.push_str(&ident);
                    result.push_str(&r.emit());
                }
            }
            _ => unimplemented!(),
        }
        result
    }
}

pub mod helpers {
    use super::*;

    pub fn int(v: i64) -> Box<Expr> {
        Box::new(Expr::Int(v))
    }

    pub fn float(v: f64) -> Box<Expr> {
        Box::new(Expr::Float(v))
    }

    pub fn bool(v: bool) -> Box<Expr> {
        Box::new(Expr::Bool(v))
    }

    pub fn string<S: Into<String>>(v: S) -> Box<Expr> {
        Box::new(Expr::String(v.into()))
    }

    pub fn empty() -> Box<Expr> {
        Box::new(Expr::Empty)
    }

    pub fn lparen() -> Box<Expr> {
        Box::new(Expr::LParen)
    }

    pub fn rparen() -> Box<Expr> {
        Box::new(Expr::RParen)
    }

    pub fn identifier<S: Into<String>>(v: S) -> Box<Expr> {
        Box::new(Expr::Ident(v.into()))
    }

    pub fn var_decl(lvalue: Box<Expr>, expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::DeclareVar(lvalue, expr))
    }

    pub fn var_assign<S: Into<String>>(ident: S, expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Assign(ident.into(), expr))
    }

    pub fn fun_call<S: Into<String>>(ident: S, args: Vec<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::FunCall(ident.into(), args))
    }

    pub fn fun_decl<S: Into<String>>(ident: S, args: Vec<Ident>, body: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::FunDecl(ident.into(), args, body))
    }

    pub fn block(exprs: Vec<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::Block(exprs))
    }

    pub fn while_loop(cond: Box<Expr>, body: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::WhileLoop(cond, body))
    }

    pub fn if_stmt(cond: Box<Expr>, pos: Box<Expr>, neg: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::If(cond, pos, neg))
    }

    pub fn ret(v: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Return(v))
    }

    pub fn add(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        fun_call("+", vec![l, r])
    }

    pub fn sub(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        fun_call("-", vec![l, r])
    }

    pub fn mul(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        fun_call("*", vec![l, r])
    }

    pub fn div(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        fun_call("/", vec![l, r])
    }

    pub fn pow(l: Box<Expr>, r: Box<Expr>) -> Box<Expr> {
        fun_call("^", vec![l, r])
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
