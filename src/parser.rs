//use pest::{Parser as PestParser, iterators::Pair};

use std::{fs::File, io::{self, Read}};

use ast::Expr;
use ast::helpers::*;
//use prec_climber::{tokenize, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Start,
    Number,
    Float,
    Ident,
    String,
    Whitespace,
    End
}

impl State {
    pub fn fold(&mut self, cs: &mut Vec<char>) -> Option<Box<Expr>> {
        let res = match *self {
            State::Start => None,
            State::Number => {
                let string: String = cs.iter().collect();
                let number: i64 = string.parse().expect("Could not parse integer");
                Some(int(number))
            },
            State::Float => {
                let string: String = cs.iter().collect();
                let number: f64 = string.parse().expect("Could not parse float");
                Some(float(number))
            },
            State::Ident => {
                Some(var(cs.iter().collect::<String>()))
            },
            State::String => {
                Some(var(cs.iter().collect::<String>()))
            },
            State::Whitespace => unreachable!("Whitespace"),
            State::End => unreachable!("End"),
        };
        if res.is_some() {
            cs.clear();
        }
        res
    }

    fn try_whitespace(&mut self, c: char, cs: &mut Vec<char>) -> Option<Box<Expr>> {
        if c.is_whitespace() {
            let res = self.fold(cs);
            *self = State::Whitespace;
            res
        } else {
            None
        }
    }

    pub fn read_char(&mut self, c: char, cs: &mut Vec<char>) -> Option<Box<Expr>> {
        match *self {
            State::End => unreachable!(),
            State::Whitespace => {
                if !c.is_whitespace() {
                    if c.is_numeric() {
                        *self = State::Number;
                        cs.clear();
                        cs.push(c);
                    } else if c == '"' {
                        *self = State::String;
                        cs.clear();
                        cs.push(c);
                    } else {
                        *self = State::Ident;
                        cs.clear();
                        cs.push(c);
                    }
                }
                None
            }
            State::Number => {
                println!("Number, {:?}, {:?}", c, cs);
                let res = self.try_whitespace(c, cs);
                if res.is_none() {
                    if c.is_numeric() {
                        cs.push(c);
                    } else if c == '.' {
                        cs.push(c);
                        *self = State::Float;
                    } else {
                        panic!("Expected number or .");
                    }
                }
                res
            }
            State::Float => {
                let res = self.try_whitespace(c, cs);
                if res.is_none() {
                    if c.is_numeric() {
                        cs.push(c);
                    } else {
                        panic!("Expected number");
                    }
                }
                res
            }
            State::Ident => {
                let res  = self.try_whitespace(c,cs);
                if res.is_none() {
                    cs.push(c);
                }
                res
            }
            State::String => {
                cs.push(c);
                if c == '"' {
                    self.fold(cs)
                } else {
                    None
                }
            }
            State::Start => {
                println!("Start, {}", c);
                if c.is_whitespace() {
                    *self = State::Whitespace;
                } else if c.is_numeric() {
                    *self = State::Number;
                    cs.clear();
                    cs.push(c);
                } else if c.is_alphabetic() {
                    *self = State::Ident;
                    cs.clear();
                    cs.push(c);
                } else if c == '"' {
                    *self = State::String;
                    cs.clear();
                    cs.push(c);
                }
                None
            }
        }
    }
}

pub struct Parser;

impl Parser {
    pub fn parse(input: &str) -> Box<Expr> {
        let mut stack: Vec<Box<Expr>> = Vec::new();
        let mut state = State::Start;
        let mut cs = Vec::new();

        let mut chars = input.char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            let expr_opt = state.read_char(c, &mut cs);
            if expr_opt.is_some() {
                stack.push(expr_opt.unwrap());
            }
        }

        let expr_opt = state.fold(&mut cs);
        if expr_opt.is_some() {
            stack.push(expr_opt.unwrap());
        }

        return block(stack);
    }
}


pub fn parse_file(filename: &str) -> io::Result<Box<Expr>> {
    let mut input = String::new();
    File::open(filename)?.read_to_string(&mut input)?;

    Ok(parse_string(&input))
}

pub fn parse_string(string: &str) -> Box<Expr> {
    let ast = Parser::parse(string);
    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    //use ast::{helpers::*, UnOp, BinOp};

    macro_rules! assert_parse {
        ($program:expr => $($e:expr),*) => {
            assert_eq!(parse_string($program), block(vec![$($e),*]));
        }
    }

    #[test]
    fn int_test() {
        assert_parse!("123" => int(123));
    }

}
