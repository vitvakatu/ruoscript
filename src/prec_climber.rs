use ast::{BinOp, Expr, UnOp};
use pest::iterators::Pair;
use parser::Rule;

use std::iter::Peekable;
use std::slice::Iter;
use to_ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Int(i64),
    Float(f64),
    Bool(bool),
    Literal(String),
    String(String),
    FunCall(String, Vec<Box<Expr>>),
}

fn tokenize_rec(pair: Pair<Rule>, tokens: &mut Vec<Token>) {
    if let Rule::bin_expr = pair.as_rule() {
        tokens.push(Token::LParen);
        for pair in pair.into_inner() {
            if let Rule::bin_expr = pair.as_rule() {
                tokenize_rec(pair, tokens);
            } else {
                let token = match pair.as_rule() {
                    Rule::int => Token::Int(pair.as_str().parse().unwrap()),
                    Rule::float => Token::Float(pair.as_str().parse().unwrap()),
                    Rule::boolean => Token::Bool(pair.as_str().parse().unwrap()),
                    Rule::identifier => Token::Literal(pair.as_str().to_string()),
                    Rule::string => {
                        let string = pair.as_str();
                        let len = string.len();
                        Token::String(string[1..len - 1].to_string())
                    }
                    Rule::fun_call => {
                        let mut inner = pair.into_inner();
                        let ident = inner.next().unwrap().as_str().to_string();
                        let args = inner.map(to_ast).collect();
                        Token::FunCall(ident, args)
                    }
                    Rule::op_add => Token::Add,
                    Rule::op_mul => Token::Mul,
                    Rule::op_sub => Token::Sub,
                    Rule::op_div => Token::Div,
                    Rule::op_minus => Token::UnMinus,
                    Rule::op_pow => Token::Pow,
                    Rule::op_not => Token::UnNot,
                    Rule::op_eq => Token::Eq,
                    Rule::op_neq => Token::Neq,
                    Rule::op_gt => Token::Gt,
                    Rule::op_lt => Token::Lt,
                    Rule::op_ge => Token::Ge,
                    Rule::op_le => Token::Le,
                    Rule::op_or => Token::Or,
                    Rule::op_and => Token::And,
                    _ => unreachable!(),
                };
                tokens.push(token);
            }
        }
        tokens.push(Token::RParen);
    } else {
        panic!("Not a binary expression");
    }
}

pub fn tokenize(pair: Pair<Rule>) -> Vec<Token> {
    let mut tokens = Vec::new();
    tokenize_rec(pair, &mut tokens);
    tokens
}

impl Token {
    pub fn lbp(&self) -> u32 {
        match *self {
            Token::Pow => 200,
            Token::UnMinus => 100,
            Token::UnNot => 100,
            Token::Mul => 80,
            Token::Div => 80,
            Token::Add => 60,
            Token::Sub => 60,
            Token::Eq => 30,
            Token::Neq => 30,
            Token::Le => 30,
            Token::Lt => 30,
            Token::Ge => 30,
            Token::Gt => 30,
            Token::And => 10,
            Token::Or => 5,
            _ => 0,
        }
    }

    pub fn nud(&self, parser: &mut Parser) -> Box<Expr> {
        match *self {
            Token::Int(i) => Box::new(Expr::Int(i)),
            Token::Float(f) => Box::new(Expr::Float(f)),
            Token::Bool(b) => Box::new(Expr::Bool(b)),
            Token::Literal(ref s) => Box::new(Expr::Variable(s.clone())),
            Token::String(ref s) => Box::new(Expr::String(s.clone())),
            Token::FunCall(ref ident, ref args) => {
                Box::new(Expr::FunCall(ident.clone(), args.clone()))
            }
            Token::UnMinus => Box::new(Expr::UnOp(UnOp::Minus, parser.expression(self.lbp()))),
            Token::UnNot => Box::new(Expr::UnOp(UnOp::Not, parser.expression(self.lbp()))),
            Token::LParen => {
                let expr = parser.expression(self.lbp());
                parser.skip_rparen();
                expr
            }
            _ => panic!("Not a int"),
        }
    }

    pub fn led(&self, parser: &mut Parser, lhs: Box<Expr>) -> Box<Expr> {
        match *self {
            Token::Add
            | Token::Mul
            | Token::Sub
            | Token::Div
            | Token::Pow
            | Token::Eq
            | Token::Neq
            | Token::Le
            | Token::Lt
            | Token::Ge
            | Token::Gt
            | Token::And
            | Token::Or => {
                let lbp = if *self == Token::Pow {
                    self.lbp() - 1
                } else {
                    self.lbp()
                };
                let rhs = parser.expression(lbp);
                let op = match *self {
                    Token::Add => BinOp::Add,
                    Token::Mul => BinOp::Mul,
                    Token::Sub => BinOp::Sub,
                    Token::Div => BinOp::Div,
                    Token::Pow => BinOp::Pow,
                    Token::Or => BinOp::Or,
                    Token::And => BinOp::And,
                    Token::Eq => BinOp::Eq,
                    Token::Neq => BinOp::NotEq,
                    Token::Ge => BinOp::Ge,
                    Token::Gt => BinOp::Gt,
                    Token::Le => BinOp::Le,
                    Token::Lt => BinOp::Lt,
                    _ => unreachable!(),
                };
                Box::new(Expr::BinOp(lhs, op, rhs))
            }
            _ => unreachable!(),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Iter<'a, Token>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn skip_rparen(&mut self) {
        if self.tokens.peek().unwrap() == &&Token::RParen {
            self.tokens.next().unwrap();
        } else {
            panic!("Trying to skip rparen, but it's not found!");
        }
    }

    pub fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.tokens.peek().map_or(false, |t| t.lbp() > rbp)
    }

    pub fn parse_nud(&mut self) -> Box<Expr> {
        self.tokens.next().unwrap().nud(self)
    }

    pub fn parse_led(&mut self, expr: Box<Expr>) -> Box<Expr> {
        self.tokens.next().unwrap().led(self, expr)
    }

    pub fn expression(&mut self, rbp: u32) -> Box<Expr> {
        let mut left = self.parse_nud();
        while self.next_binds_tighter_than(rbp) {
            left = self.parse_led(left);
        }
        left
    }
}
