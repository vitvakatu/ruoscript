type Ident = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    Variable(Ident),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    Assign(Ident, Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Gt,
    Lt,
    Ge,
    Le,
    NotEq,
    Eq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    Not,
    Minus,
}