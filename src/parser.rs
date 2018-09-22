//use pest::{Parser as PestParser, iterators::Pair};

use std::{fs::File, io::{self, Read}};

use ast::Expr;
use ast::helpers::*;
//use prec_climber::{tokenize, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum FloatPart {
    Fraction,
    Exponent
}

pub type Error = String;

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Start,
    Number,
    Float(FloatPart),
    Ident,
    String,
    Whitespace,
    End
}

impl State {
    pub fn fold(&mut self, cs: &mut Vec<char>) -> Result<Box<Expr>, Error> {
        let res = match *self {
            State::Start => unreachable!("Start"),
            State::Number => {
                let mut string: String = cs.iter().collect();
                let number: i64 = match string.parse() {
                    Ok(n) => n,
                    Err(e) => return Err(format!("Unable to parse integer: {}", e))
                };
                Ok(int(number))
            },
            State::Float(_) => {
                let string: String = cs.iter().collect();
                let number: f64 = match string.parse() {
                    Ok(n) => n,
                    Err(e) => return Err(format!("Unable to parse float: {}", e))
                };
                Ok(float(number))
            },
            State::Ident => {
                Ok(ident(cs.iter().collect::<String>()))
            },
            State::String => {
                Ok(string(cs.iter().skip(1).collect::<String>()))
            },
            State::Whitespace => unreachable!("Whitespace"),
            State::End => unreachable!("End"),
        };
        if res.is_ok() {
            cs.clear();
        }
        res
    }

    fn try_whitespace(&mut self, c: char, cs: &mut Vec<char>) -> Result<Option<Box<Expr>>, Error> {
        if c.is_whitespace() {
            let res = self.fold(cs);
            *self = State::Whitespace;
            res.map(Some)
        } else {
            Ok(None)
        }
    }

    pub fn read_char(&mut self, c: char, nc: Option<char>, cs: &mut Vec<char>) -> Result<Option<Box<Expr>>, Error> {
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
                Ok(None)
            }
            State::Number => {
                let res = self.try_whitespace(c, cs)?;
                if res.is_none() {
                    if c.is_numeric() {
                        cs.push(c);
                    } else if c == '_' {
                        // do nothing
                    } else if c == '.' {
                        cs.push(c);
                        *self = State::Float(FloatPart::Fraction);
                    } else {
                        return Err("Expected number or .".into());
                    }
                }
                Ok(res)
            }
            State::Float(FloatPart::Fraction) => {
                let res = self.try_whitespace(c, cs)?;
                if res.is_none() {
                    if c.is_numeric() {
                        cs.push(c);
                    } else if c == 'e' || c == 'E' {
                        cs.push(c);
                        *self = State::Float(FloatPart::Exponent);
                    } else {
                        return Err("Expected number or e or E".into());
                    }
                }
                Ok(res)
            }
            State::Float(FloatPart::Exponent) => {
                let res = self.try_whitespace(c, cs)?;
                if res.is_none() {
                    if c.is_numeric() || c == '-' || c == '+' {
                        cs.push(c);
                    } else {
                        return Err("Expected number or -".into());
                    }
                }
                Ok(res)
            }
            State::Ident => {
                let res  = self.try_whitespace(c,cs)?;
                if res.is_none() {
                    cs.push(c);
                }
                Ok(res)
            }
            State::String => {
                if c == '"' {
                    self.fold(cs).map(Some)
                } else {
                    cs.push(c);
                    Ok(None)
                }
            }
            State::Start => {
                if c.is_whitespace() {
                    *self = State::Whitespace;
                } else if c.is_numeric() || (c == '-' && nc.map_or(false, |c| c.is_numeric())) {
                    *self = State::Number;
                    cs.clear();
                    cs.push(c);
                } else if c == '"' {
                    *self = State::String;
                    cs.clear();
                    cs.push(c);
                } else if c == '.' {
                    return Err("Found illegal identifier".into());
                } else {
                    *self = State::Ident;
                    cs.clear();
                    cs.push(c);
                }
                Ok(None)
            }
        }
    }
}

pub struct Parser;

impl Parser {
    pub fn parse(input: &str) -> Result<Box<Expr>, Error> {
        let mut stack: Vec<Box<Expr>> = Vec::new();
        let mut state = State::Start;
        let mut cs = Vec::new();

        let mut chars = input.char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            let nc = chars.peek().map(|(i, c)| *c);
            let expr_opt = state.read_char(c, nc, &mut cs)?;
            if expr_opt.is_some() {
                stack.push(expr_opt.unwrap());
            }
        }

        if !cs.is_empty() {
            let expr = state.fold(&mut cs)?;
            stack.push(expr);
        }

        Ok(block(stack))
    }
}


pub fn parse_file(filename: &str) -> io::Result<Box<Expr>> {
    let mut input = String::new();
    File::open(filename)?.read_to_string(&mut input)?;

    Ok(parse_string(&input).unwrap())
}

pub fn parse_string(string: &str) -> Result<Box<Expr>, Error> {
    let ast = Parser::parse(string);
    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    //use ast::{helpers::*, UnOp, BinOp};

    macro_rules! assert_parse {
        ($program:expr => $($e:expr),*) => {
            assert_eq!(parse_string($program).unwrap(), block(vec![$($e),*]));
        }
    }

    macro_rules! assert_not_parse {
        ($program:expr) => {
            assert!(parse_string($program).is_err());
        }
    }

    #[test]
    fn int_test() {
        assert_parse!("123" => int(123));
        assert_parse!("0" => int(0));
        assert_parse!("1" => int(1));
        assert_parse!("-1" => int(-1));
        assert_parse!("-0" => int(0));
        assert_parse!("3_000_000" => int(3_000_000));
        assert_not_parse!("1-23");
    }

    #[test]
    fn float_test() {
        assert_parse!("0.0" => float(0.0));
        assert_parse!("-0.0" => float(0.0));
        assert_parse!("3.14" => float(3.14));
        assert_parse!("-3.14" => float(-3.14));
        assert_parse!("0.16e-3" => float(0.16e-3));
        assert_parse!("0.16e1" => float(1.6));
        assert_parse!("0.16e0" => float(0.16));
        assert_parse!("0.16E-3" => float(0.16e-3));
        assert_parse!("1.6e7" => float(1.6e7));
        assert_parse!("1.6e+7" => float(1.6e7));
        assert_parse!("1.6E7" => float(1.6e7));
        assert_parse!("-1.6e7" => float(-1.6e7));
        assert_not_parse!(".0");
        assert_not_parse!("3.14B2");
        assert_not_parse!(".3");
        assert_not_parse!("0.0.4");
        assert_not_parse!("-0.03-3");
        assert_not_parse!("-0.03e-3-2");
        assert_not_parse!("-0.03e-3+2");
    }

    #[test]
    fn ident_test() {
        assert_parse!("a" => ident("a"));
        assert_parse!("abc" => ident("abc"));
        assert_parse!("a123" => ident("a123"));
        assert_parse!("_213" => ident("_213"));
        assert_parse!("_" => ident("_"));
        assert_parse!("+" => ident("+"));
        assert_parse!("-" => ident("-"));
        assert_parse!(">=" => ident(">="));
        assert_parse!("∮" => ident("∮"));
    }

    #[test]
    fn string_test() {
        assert_parse!("\"\"" => string(""));
        assert_parse!("\"Hello\"" => string("Hello"));
        assert_parse!("\"∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i)\"" => string("∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i)"));
    }
}
