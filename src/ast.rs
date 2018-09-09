type Ident = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Empty,
    Variable(Ident),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    DeclareVar(Ident, Box<Expr>),
    Assign(Ident, Box<Expr>),
    FunCall(Ident, Vec<Box<Expr>>),
    Return(Box<Expr>),
    Block(Vec<Box<Expr>>),
    WhileLoop(Box<Expr>, Box<Expr>),
    FunDecl(Ident, Vec<Ident>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmpOp {
    Lt,
    Le,
    Gt,
    Ge,
    NotEq,
    Eq,
}
