use ast::{Expr, BinOp, UnOp};
use pest::iterators::{Pairs, Pair};
use Rule;

use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug)]
pub enum Token {
    Add,
    Mul,
    Int(i64),
}

pub fn tokenize(pair: Pair<Rule>) -> Vec<Token> {
    let mut tokens = Vec::new();
    if let Rule::bin_expr = pair.as_rule() {
        for pair in pair.into_inner() {
            let token = match pair.as_rule() {
                Rule::int => Token::Int(pair.as_str().parse().unwrap()),
                Rule::op_add => Token::Add,
                Rule::op_mul => Token::Mul,
                _ => unimplemented!()
            };
            tokens.push(token);
        }
    } else {
        panic!("Not a binary expression");
    }
    tokens
}

impl Token {
    pub fn lbp(&self) -> u32 {
        match *self {
            Token::Mul => 20,
            Token::Add => 10,
            _ => 0,
        }
    }

    pub fn nud(&self) -> Box<Expr> {
        match *self {
            Token::Int(i) => Box::new(Expr::Int(i)),
            _ => panic!("Not a int"),
        }
    }

    pub fn led(&self, parser: &mut Parser, lhs: Box<Expr>) -> Box<Expr> {
        match *self {
            Token::Add | Token::Mul => {
                let rhs = parser.expression(self.lbp());
                let op = match *self {
                    Token::Add => BinOp::Add,
                    Token::Mul => BinOp::Mul,
                    _ => unreachable!()
                };
                Box::new(Expr::BinOp(lhs, op, rhs))
            }
            _ => unreachable!()
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Iter<'a, Token>) -> Self {
        Self { tokens: tokens.peekable() }
    }

    pub fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.tokens.peek().map_or(false, |t| { t.lbp() > rbp })
    }

    pub fn parse_nud(&mut self) -> Box<Expr> {
        self.tokens.next().unwrap().nud()
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