use ast::{Expr, BinOp, UnOp};
use pest::iterators::{Pairs, Pair};
use Rule;

use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Add,
    Mul,
    Sub,
    Div,
    Pow,
    UnMinus,
    UnNot,
    Eq,
    Neq,
    Ge,
    Gt,
    Le,
    Lt,
    LParen,
    RParen,
    Int(i64),
    Float(f64),
    Literal(String),
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
                    Rule::identifier => Token::Literal(pair.as_str().to_string()),
                    Rule::op_add => Token::Add,
                    Rule::op_mul => Token::Mul,
                    Rule::op_sub => Token::Sub,
                    Rule::op_div => Token::Div,
                    Rule::op_minus => Token::UnMinus,
                    _ => unimplemented!()
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
    println!("{:?}", tokens);
    tokens
}

impl Token {
    pub fn lbp(&self) -> u32 {
        match *self {
            Token::Mul => 20,
            Token::Add => 10,
            Token::Sub => 10,
            Token::UnMinus => 10,
            Token::LParen => 0,
            Token::RParen => 0,
            _ => 0,
        }
    }

    pub fn nud(&self, parser: &mut Parser) -> Box<Expr> {
        match *self {
            Token::Int(i) => Box::new(Expr::Int(i)),
            Token::Literal(ref s) => Box::new(Expr::Variable(s.clone())),
            Token::UnMinus => Box::new(Expr::UnOp(UnOp::Minus, parser.expression(100))),
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
            Token::Add | Token::Mul | Token::Sub => {
                let rhs = parser.expression(self.lbp());
                let op = match *self {
                    Token::Add => BinOp::Add,
                    Token::Mul => BinOp::Mul,
                    Token::Sub => BinOp::Sub,
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

    pub fn skip_rparen(&mut self) {
        if self.tokens.peek().unwrap() == &&Token::RParen {
            self.tokens.next().unwrap();
        } else {
            panic!("Trying to skip rparen, but it's not found!");
        }
    }

    pub fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.tokens.peek().map_or(false, |t| { t.lbp() > rbp })
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