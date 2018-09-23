//use pest::{Parser as PestParser, iterators::Pair};

use std::{
    fs::File,
    io::{self, Read},
};

use ast::helpers::*;
use ast::Expr;
use climber::Climber;

#[derive(Debug, Clone, PartialEq)]
pub enum FloatPart {
    Fraction,
    Exponent,
}

pub type Error = String;

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Start,
    Number,
    Float(FloatPart),
    Ident,
    OperatorIdent,
    String,
    Whitespace,
    End,
}

fn is_valid_ident_symbol(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_valid_first_ident_symbol(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

impl State {
    pub fn fold(&mut self, cs: &mut Vec<char>) -> Result<Option<Box<Expr>>, Error> {
        info!("Folding: {:?}", cs.iter().collect::<String>());
        let res = match *self {
            State::Start => Ok(None),
            State::Number => {
                let mut string: String = cs.iter().collect();
                let number: i64 = match string.parse() {
                    Ok(n) => n,
                    Err(e) => return Err(format!("Unable to parse integer: {}", e)),
                };
                Ok(Some(int(number)))
            }
            State::Float(_) => {
                let string: String = cs.iter().collect();
                let number: f64 = match string.parse() {
                    Ok(n) => n,
                    Err(e) => return Err(format!("Unable to parse float: {}", e)),
                };
                Ok(Some(float(number)))
            }
            State::Ident => Ok(Some(identifier(cs.iter().collect::<String>()))),
            State::OperatorIdent => Ok(Some(identifier(cs.iter().collect::<String>()))),
            State::String => Ok(Some(string(cs.iter().skip(1).collect::<String>()))),
            State::Whitespace => Ok(None),
            State::End => Ok(None),
        };
        if res.is_ok() {
            cs.clear();
        }
        info!("Folding result: {:?}", res);
        res
    }

    fn try_whitespace(&mut self, c: char, cs: &mut Vec<char>) -> Result<Option<Box<Expr>>, Error> {
        if c.is_whitespace() {
            let res = self.fold(cs);
            *self = State::Whitespace;
            res
        } else {
            Ok(None)
        }
    }

    fn try_parens(&mut self, c: char, paren_counter: &mut u8) -> Result<Option<Box<Expr>>, Error> {
        if c == '(' {
            *paren_counter += 1;
            Ok(Some(lparen()))
        } else if c == ')' {
            if *paren_counter < 1 {
                return Err("Unmatched parenthesis found: closing parens count is greater".into());
            }
            *paren_counter -= 1;
            Ok(Some(rparen()))
        } else {
            Ok(None)
        }
    }

    pub fn read_char(
        &mut self,
        stack: &mut Vec<Box<Expr>>,
        c: char,
        nc: Option<char>,
        cs: &mut Vec<char>,
        paren_counter: &mut u8,
    ) -> Result<(), Error> {
        info!("Reading char: {}", c);
        if let Some(paren) = self.try_parens(c, paren_counter)? {
            match *self {
                State::String => {}
                _ => {
                    if let Some(res) = self.fold(cs)? {
                        stack.push(res);
                    }
                    stack.push(paren);
                    *self = State::Start;
                    return Ok(());
                }
            }
        }
        info!("Current state: {:?}", self);
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
                    } else if is_valid_first_ident_symbol(c) {
                        *self = State::Ident;
                        cs.clear();
                        cs.push(c);
                    } else {
                        *self = State::OperatorIdent;
                        cs.clear();
                        cs.push(c);
                    }
                }
                Ok(())
            }
            State::Number => {
                let res = self.try_whitespace(c, cs)?;
                if let Some(res) = res {
                    stack.push(res);
                    Ok(())
                } else {
                    if c.is_numeric() {
                        cs.push(c);
                    } else if c == '_' {
                        // do nothing
                    } else if c == '.' {
                        cs.push(c);
                        *self = State::Float(FloatPart::Fraction);
                    } else if !is_valid_first_ident_symbol(c) {
                        stack.push(self.fold(cs)?.unwrap());
                        *self = State::OperatorIdent;
                        cs.push(c);
                    } else {
                        return Err("Found illegal integer number".into());
                    }
                    Ok(())
                }
            }
            State::Float(FloatPart::Fraction) => {
                let res = self.try_whitespace(c, cs)?;
                if let Some(res) = res {
                    stack.push(res);
                    Ok(())
                } else {
                    if c.is_numeric() {
                        cs.push(c);
                    } else if c == 'e' || c == 'E' {
                        cs.push(c);
                        *self = State::Float(FloatPart::Exponent);
                    } else if !is_valid_first_ident_symbol(c) {
                        stack.push(self.fold(cs)?.unwrap());
                        *self = State::OperatorIdent;
                        cs.push(c);
                    } else {
                        return Err("Found illegal floating point number".into());
                    }
                    Ok(())
                }
            }
            State::Float(FloatPart::Exponent) => {
                let res = self.try_whitespace(c, cs)?;
                if let Some(res) = res {
                    stack.push(res);
                    Ok(())
                } else {
                    if c.is_numeric() || c == '-' || c == '+' {
                        cs.push(c);
                    } else if !is_valid_first_ident_symbol(c) {
                        stack.push(self.fold(cs)?.unwrap());
                        *self = State::OperatorIdent;
                        cs.push(c);
                    } else {
                        return Err("Found illegal floating point number".into());
                    }
                    Ok(())
                }
            }
            State::Ident => {
                let res = self.try_whitespace(c, cs)?;
                if let Some(res) = res {
                    stack.push(res);
                    Ok(())
                } else if !is_valid_ident_symbol(c) {
                    stack.push(self.fold(cs)?.unwrap());
                    *self = State::OperatorIdent;
                    cs.push(c);
                    Ok(())
                } else {
                    cs.push(c);
                    Ok(())
                }
            }
            State::OperatorIdent => {
                let res = self.try_whitespace(c, cs)?;
                if let Some(res) = res {
                    stack.push(res);
                    Ok(())
                } else if c.is_numeric() {
                    stack.push(self.fold(cs)?.unwrap());
                    *self = State::Number;
                    cs.push(c);
                    Ok(())
                } else if is_valid_ident_symbol(c) {
                    stack.push(self.fold(cs)?.unwrap());
                    *self = State::Ident;
                    cs.push(c);
                    Ok(())
                } else {
                    cs.push(c);
                    Ok(())
                }
            }
            State::String => {
                if c == '"' {
                    stack.push(self.fold(cs)?.unwrap());
                } else {
                    cs.push(c);
                }
                Ok(())
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
                } else if is_valid_first_ident_symbol(c) {
                    *self = State::Ident;
                    cs.clear();
                    cs.push(c);
                } else if c == '.' {
                    return Err("Found unexpected '.'".into());
                } else {
                    *self = State::OperatorIdent;
                    cs.clear();
                    cs.push(c);
                }
                Ok(())
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
        let mut paren_count = 0;

        info!("Parsing input: {}", input);

        let mut chars = input.char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            let nc = chars.peek().map(|(i, c)| *c);
            state.read_char(&mut stack, c, nc, &mut cs, &mut paren_count)?;
        }

        if !cs.is_empty() {
            let expr = state.fold(&mut cs)?;
            stack.push(expr.unwrap());
        }

        if !stack.is_empty() {
            let mut climber = Climber::new(stack.iter());
            let ast = climber.expression(0);
            Ok(block(vec![ast]))
        } else {
            Ok(block(vec![]))
        }
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
    extern crate env_logger;
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
        };
    }

    #[test]
    fn empty() {
        assert_parse!("" => );
    }

    #[test]
    fn int_test() {
        assert_parse!("123" => int(123));
        assert_parse!("0" => int(0));
        assert_parse!("1" => int(1));
        assert_parse!("-1" => int(-1));
        assert_parse!("-0" => int(0));
        assert_parse!("3_000_000" => int(3_000_000));
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
    }

    #[test]
    fn ident_test() {
        assert_parse!("a" => identifier("a"));
        assert_parse!("abc" => identifier("abc"));
        assert_parse!("a123" => identifier("a123"));
        assert_parse!("_213" => identifier("_213"));
        assert_parse!("_" => identifier("_"));
        assert_parse!("+" => identifier("+"));
        assert_parse!(">=" => identifier(">="));
        assert_parse!("∮" => identifier("∮"));
    }

    #[test]
    fn string_test() {
        assert_parse!("\"\"" => string(""));
        assert_parse!("\"Hello\"" => string("Hello"));
        assert_parse!("\"∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i)\"" => string("∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i)"));
    }

    #[test]
    fn unary_exprs() {
        assert_parse!("- a" => fun_call("-", vec![identifier("a")]));
    }

    #[test]
    fn binary_exprs() {
        assert_parse!("1 + 2" => add(int(1), int(2)));
        assert_parse!("1+2" => add(int(1), int(2)));
        assert_parse!("1 + a" => add(int(1), identifier("a")));
        assert_parse!("a + 2" => add(identifier("a"), int(2)));
        assert_parse!("a + b" => add(identifier("a"), identifier("b")));
        assert_parse!("a+b" => add(identifier("a"), identifier("b")));
        assert_parse!("1 - 2" => sub(int(1), int(2)));
        assert_parse!("(1 - 2)" => sub(int(1), int(2)));
        assert_parse!("1 + 3 * 4" => add(int(1), mul(int(3), int(4))));
        assert_parse!("1 * 3 + 4" => add(mul(int(1), int(3)), int(4)));
        assert_parse!("1*3+4" => add(mul(int(1), int(3)), int(4)));
        assert_parse!("1 - - b" => sub(int(1), fun_call("-", vec![identifier("b")])));
        assert_parse!("3 * (8 - 5)" => mul(int(3), sub(int(8), int(5))));
        assert_parse!("3 - (8 * 5)" => sub(int(3), mul(int(8), int(5))));
        assert_parse!("3-(8*5)" => sub(int(3), mul(int(8), int(5))));
        assert_parse!("3 - 8 * 5" => sub(int(3), mul(int(8), int(5))));
        assert_parse!("2 ^ 3 ^ 4" => pow(int(2), pow(int(3), int(4))));
        assert_parse!("a^b^c" => pow(identifier("a"), pow(identifier("b"), identifier("c"))));
    }
}
