use ast::Expr;

use std::iter::Peekable;
use std::slice::Iter;

use ast::helpers::*;

fn get_lbp(func: &str) -> u32 {
    match func {
        "+" => 60,
        "*" => 80,
        "-" => 60,
        _ => 0,
    }
}

fn is_unary(func: &str) -> bool {
    match func {
        "-" => true,
        _ => false,
    }
}

fn is_binary(func: &str) -> bool {
    match func {
        "+" => true,
        "-" => true,
        "*" => true,
        _ => false,
    }
}

impl Expr {
    pub fn nud(&self, parser: &mut Climber) -> Box<Expr> {
        match *self {
            Expr::Ident(ref ident) => {
                if is_unary(&ident) {
                    fun_call(ident.clone(), vec![parser.expression(get_lbp(&ident))])
                } else {
                    identifier(ident.clone())
                }
            }
            ref expr => Box::new(expr.clone()),
        }
    }

    pub fn led(&self, parser: &mut Climber, lhs: Box<Expr>) -> Box<Expr> {
        match *self {
            Expr::Ident(ref ident) => {
                let lbp = get_lbp(&ident);
                let rhs = parser.expression(lbp);
                fun_call(ident.clone(), vec![lhs, rhs])
            }
            _ => unreachable!(),
        }
    }
}

pub struct Climber<'a> {
    tokens: Peekable<Iter<'a, Box<Expr>>>,
}

impl<'a> Climber<'a> {
    pub fn new(tokens: Iter<'a, Box<Expr>>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.tokens.peek().map_or(false, |t| {
            if let Expr::Ident(ref ident) = ***t {
                get_lbp(&ident) > rbp
            } else {
                false
            }
        })
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
