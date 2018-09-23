use ast::Expr;

use std::iter::Peekable;
use std::slice::Iter;

use ast::helpers::*;

fn get_lbp(func: &str) -> u32 {
    match func {
        "^" => 200,
        "!" => 100,
        "/" => 80,
        "*" => 80,
        "+" => 60,
        "-" => 60,
        "==" => 30,
        ">=" => 30,
        "<=" => 30,
        "!=" => 30,
        "<" => 30,
        ">" => 30,
        "&&" => 10,
        "||" => 5,
        _ => 0,
    }
}

fn is_unary(func: &str) -> bool {
    match func {
        "-" => true,
        "!" => true,
        "return" => true,
        _ => false,
    }
}

fn is_binary(func: &str) -> bool {
    match func {
        "^" => true,
        "/" => true,
        "*" => true,
        "+" => true,
        "-" => true,
        "==" => true,
        ">=" => true,
        "<=" => true,
        "!=" => true,
        "<" => true,
        ">" => true,
        "&&" => true,
        "||" => true,
        _ => false,
    }
}

fn is_right_assoc(func: &str) -> bool {
    func == "^"
}

impl Expr {
    pub fn nud(&self, parser: &mut Climber) -> Box<Expr> {
        match *self {
            Expr::Ident(ref ident) => {
                if is_unary(&ident) {
                    if ident == "return" {
                        ret(parser.expression(get_lbp(&ident)))
                    } else {
                        fun_call(ident.clone(), vec![parser.expression(get_lbp(&ident))])
                    }
                } else {
                    identifier(ident.clone())
                }
            }
            Expr::LParen => {
                let expr = parser.expression(0);
                parser.skip_rparen();
                expr
            }
            ref expr => Box::new(expr.clone()),
        }
    }

    pub fn led(&self, parser: &mut Climber, lhs: Box<Expr>) -> Box<Expr> {
        match *self {
            Expr::Ident(ref ident) => {
                let lbp = if is_right_assoc(&ident) {
                    get_lbp(&ident) - 1
                } else {
                    get_lbp(&ident)
                };
                let rhs = parser.expression(lbp);
                fun_call(ident.clone(), vec![lhs, rhs])
            }
            _ => unreachable!("Led"),
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
            let lbp = if let Expr::Ident(ref ident) = ***t {
                get_lbp(&ident)
            } else {
                0
            };
            lbp > rbp
        })
    }

    pub fn skip_rparen(&mut self) {
        if let Some(expr) = self.tokens.peek().cloned() {
            if let Expr::RParen = **expr {
                self.tokens.next().unwrap();
            } else {
                panic!("Trying to skip RParen, but it's not found");
            }
        } else {
            panic!("Trying to skip RParen, but there is no token next");
        }
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
