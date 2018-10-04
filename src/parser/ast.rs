#[derive(Debug, PartialEq, Clone)]
pub struct Module(pub Vec<TopLevelStatement>);

/// Function prototype
#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Block {
    Void(Vec<Statement>),
    NonVoid(Vec<Statement>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelStatement {
    Prototype(Prototype),
    Function(Prototype, Block),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expr(Box<Expr>),
    VariableDeclaration(String, Box<Expr>),
    Return(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i32),
    String(String),
    Variable(String),
    /// Function call (function name, arguments)
    Call(String, Vec<Box<Expr>>),
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

    pub fn var_declaration<S: Into<String>>(name: S, expr: Box<Expr>) -> Statement {
        Statement::VariableDeclaration(name.into(), expr)
    }

    pub fn ret(expr: Box<Expr>) -> Statement {
        Statement::Return(expr)
    }

    pub fn statement(expr: Box<Expr>) -> Statement {
        Statement::Expr(expr)
    }

    pub fn call<S: Into<String>>(ident: S, args: Vec<Box<Expr>>) -> Box<Expr> {
        Box::new(Expr::Call(ident.into(), args))
    }

    pub fn top_level_function(proto: Prototype, block: Block) -> TopLevelStatement {
        TopLevelStatement::Function(proto, block)
    }

    pub fn top_level_proto(proto: Prototype) -> TopLevelStatement {
        TopLevelStatement::Prototype(proto)
    }

    pub fn prototype<S: Into<String>>(ident: S, args: Vec<String>) -> Prototype {
        Prototype {
            name: ident.into(),
            args,
        }
    }

    pub fn block(statements: Vec<Statement>, expr: Box<Expr>) -> Block {
        Block::NonVoid(statements, expr)
    }

    pub fn block_void(statements: Vec<Statement>) -> Block {
        Block::Void(statements)
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
