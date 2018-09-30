pub mod ast;

use self::ast::{helpers::*, Expr, Prototype};
use super::lexer::{Lexer, Span, Token};

use codegen::{Codegen, Context};

use failure::Error;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Clone, Copy, PartialEq)]
enum ExpectedToken {
    Integer,
    Identifier,
    RoundLeft,
    RoundRight,
    CurlyLeft,
    CurlyRight,
    Operator,
}

impl ::std::fmt::Debug for ExpectedToken {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        match *self {
            ExpectedToken::Integer => write!(f, "integer"),
            ExpectedToken::Identifier => write!(f, "identifier"),
            ExpectedToken::RoundLeft => write!(f, "'('"),
            ExpectedToken::RoundRight => write!(f, "')'"),
            ExpectedToken::CurlyLeft => write!(f, "'{{'"),
            ExpectedToken::CurlyRight => write!(f, "'}}'"),
            ExpectedToken::Operator => write!(f, "operator"),
        }
    }
}

#[derive(Fail, Debug)]
enum ParserError {
    #[fail(display = "Expected {:?}, found {:?}", _0, _1)]
    Expected(ExpectedToken, Span<Token>),
    #[fail(
        display = "Invalid arguments list, expected ',' or ')', found: {:?}",
        _0
    )]
    InvalidArguments(Span<Token>),
    #[fail(
        display = "Could not parse primary expression. Expected literal, function call or '('. Found: {:?}",
        _0
    )]
    CouldNotParsePrimary(Span<Token>),
    #[fail(display = "Unexpected top level token: {:?}", _0)]
    UnexpectedTopLevel(Span<Token>),
    #[fail(display = "Unexpected end of input")]
    Eof,
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Span<Token>>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Iter<'a, Span<Token>>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn next_token(&mut self) -> Option<Span<Token>> {
        self.tokens.next().cloned()
    }

    fn peek_next_token(&mut self) -> Option<Span<Token>> {
        self.tokens.peek().cloned().cloned()
    }

    fn parse_integer(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing integer");
        match self.next_token() {
            Some(Span {
                inner: Token::Integer(i),
                ..
            }) => Ok(int(i)),
            Some(other) => Err(ParserError::Expected(ExpectedToken::Integer, other))?,
            None => Err(ParserError::Eof)?,
        }
    }

    fn parse_paren_expression(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing paren expression");
        // skip '('
        let _ = self.next_token();
        let expr = self.parse_expression()?;
        match self.peek_next_token() {
            Some(Span {
                inner: Token::RoundRight,
                ..
            }) => {
                // skip ')'
                let _ = self.next_token();
            }
            Some(other) => Err(ParserError::Expected(ExpectedToken::RoundRight, other))?,
            None => Err(ParserError::Eof)?,
        }
        debug!("parsed paren expression: {:?}", expr);
        Ok(expr)
    }

    fn parse_identifier(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing identifier");
        let identifier = match self.next_token() {
            Some(Span {
                inner: Token::Identifier(identifier),
                ..
            }) => Ok(identifier),
            Some(other) => Err(ParserError::Expected(ExpectedToken::Identifier, other)),
            None => Err(ParserError::Eof),
        }?;
        match self.peek_next_token() {
            Some(Span {
                inner: Token::RoundLeft,
                ..
            }) => {
                debug!("found function call");
                // skip '('
                let _ = self.next_token();
                // parse arguments
                let mut args = Vec::new();
                // empty arguments list
                if let Some(Span {
                    inner: Token::RoundRight,
                    ..
                }) = self.peek_next_token()
                {
                    let _ = self.next_token();
                    return Ok(call(identifier, args));
                }
                loop {
                    let arg = self.parse_expression()?;
                    args.push(arg);

                    match self.peek_next_token() {
                        Some(Span {
                            inner: Token::RoundRight,
                            ..
                        }) => {
                            let _ = self.next_token();
                            break;
                        }
                        Some(Span {
                            inner: Token::Comma,
                            ..
                        }) => {
                            let _ = self.next_token();
                        }
                        Some(other) => Err(ParserError::InvalidArguments(other))?,
                        None => Err(ParserError::Eof)?,
                    }
                }
                debug!("parsed function call: {}, {:?}", identifier, args);
                Ok(call(identifier, args))
            }
            _ => Ok(variable(identifier)),
        }
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing primary");
        match self.peek_next_token() {
            Some(Span {
                inner: Token::Identifier(_),
                ..
            }) => self.parse_identifier(),
            Some(Span {
                inner: Token::Integer(_),
                ..
            }) => self.parse_integer(),
            Some(Span {
                inner: Token::RoundLeft,
                ..
            }) => self.parse_paren_expression(),
            Some(other) => Err(ParserError::CouldNotParsePrimary(other))?,
            None => Err(ParserError::Eof)?,
        }
    }

    fn parse_binary_operator(
        &mut self,
        expr_precedence: u32,
        mut lhs: Box<Expr>,
    ) -> Result<Box<Expr>, Error> {
        debug!("parsing binary operator");
        loop {
            debug!("lhs = {:?}", lhs);
            let token_precedence = self.get_token_precedence()?;

            if token_precedence <= expr_precedence {
                debug!("parsed binary expresion: {:?}", lhs);
                return Ok(lhs);
            }

            let operator = match self.next_token() {
                Some(Span {
                    inner: Token::Operator(op),
                    ..
                }) => op,
                Some(Span {
                    inner: Token::Eof, ..
                })
                | Some(Span {
                    inner: Token::NewLine,
                    ..
                }) => return Ok(lhs),
                Some(other) => Err(ParserError::Expected(ExpectedToken::Operator, other))?,
                None => Err(ParserError::Eof)?,
            };

            debug!("operator = {}", operator);
            let mut rhs = self.parse_primary()?;
            debug!("rhs = {:?}", rhs);

            let next_precedence = self.get_token_precedence()?;
            if token_precedence < next_precedence {
                rhs = self.parse_binary_operator(token_precedence + 1, rhs)?;
            }

            lhs = call(operator, vec![lhs, rhs]);
        }
    }

    fn parse_expression(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing expression");
        let lhs = self.parse_primary()?;
        self.parse_binary_operator(0, lhs)
    }

    fn parse_prototype(&mut self) -> Result<Prototype, Error> {
        debug!("parsing prototype");
        let name = match self.next_token() {
            Some(Span {
                inner: Token::Identifier(s),
                ..
            }) => s,
            Some(other) => Err(ParserError::Expected(ExpectedToken::Identifier, other))?,
            None => Err(ParserError::Eof)?,
        };

        match self.next_token() {
            Some(Span {
                inner: Token::RoundLeft,
                ..
            }) => {
                // parse arguments
                let mut args = Vec::new();
                // empty arguments list
                if let Some(Span {
                    inner: Token::RoundRight,
                    ..
                }) = self.peek_next_token()
                {
                    let _ = self.next_token();
                    return Ok(Prototype { name, args });
                }
                loop {
                    let arg = match self.next_token() {
                        Some(Span {
                            inner: Token::Identifier(s),
                            ..
                        }) => s,
                        Some(other) => {
                            Err(ParserError::Expected(ExpectedToken::Identifier, other))?
                        }
                        None => Err(ParserError::Eof)?,
                    };
                    args.push(arg);

                    match self.peek_next_token() {
                        Some(Span {
                            inner: Token::RoundRight,
                            ..
                        }) => {
                            let _ = self.next_token();
                            break;
                        }
                        Some(Span {
                            inner: Token::Comma,
                            ..
                        }) => {
                            let _ = self.next_token();
                        }
                        Some(other) => Err(ParserError::InvalidArguments(other))?,
                        None => Err(ParserError::Eof)?,
                    }
                }
                Ok(Prototype { name, args })
            }

            Some(other) => Err(ParserError::Expected(ExpectedToken::RoundLeft, other))?,
            None => Err(ParserError::Eof)?,
        }
    }

    fn parse_definition(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing definition");
        // skip 'def'
        let _ = self.next_token();

        let proto = self.parse_prototype()?;

        debug!("parsed proto: {:?}", proto);

        match self.next_token() {
            Some(Span {
                inner: Token::CurlyLeft,
                ..
            }) => {
                let body = self.parse_expression()?;
                let res = function(proto, body);
                match self.next_token() {
                    Some(Span {
                        inner: Token::CurlyRight,
                        ..
                    }) => Ok(res),
                    Some(other) => Err(ParserError::Expected(ExpectedToken::CurlyRight, other))?,
                    None => Err(ParserError::Eof)?,
                }
            }
            Some(other) => Err(ParserError::Expected(ExpectedToken::CurlyLeft, other))?,
            None => Err(ParserError::Eof)?,
        }
    }

    fn parse_extern(&mut self) -> Result<Prototype, Error> {
        debug!("parsing extern");
        // skip 'extern'
        let _ = self.next_token();
        self.parse_prototype()
    }

    pub fn parse(&mut self, context: &mut Context) -> Result<(), Error> {
        while let Some(token) = self.peek_next_token() {
            match token.inner {
                Token::Eof => return Ok(()),
                Token::NewLine => {
                    self.next_token();
                }
                Token::Def => {
                    let expr = self.parse_definition()?;
                    debug!("Parsed definition: {:?}", expr);
                    Self::codegen(expr, context);
                }
                Token::Extern => {
                    let expr = self
                        .parse_extern()
                        .map(|proto| prototype(proto.name, proto.args))?;
                    debug!("Parsed extern: {:?}", expr);
                    Self::codegen(expr, context);
                }
                _ => Err(ParserError::UnexpectedTopLevel(token))?,
            }
        }
        Ok(())
    }

    fn codegen(expr: Box<Expr>, context: &mut Context) {
        let value = expr.codegen(context);
        let string =
            unsafe { ::std::ffi::CStr::from_ptr(::llvm_sys::core::LLVMPrintValueToString(value)) };
        debug!("{}", string.to_str().unwrap());
    }

    fn get_token_precedence(&mut self) -> Result<u32, Error> {
        match self.peek_next_token() {
            Some(Span {
                inner: Token::Operator(s),
                ..
            }) => Ok(match s.as_str() {
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
            }),
            Some(_) => Ok(0),
            None => Err(ParserError::Eof)?,
        }
    }
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
        let _ = env_logger::try_init();
        assert_parse!("" => );
    }

    #[test]
    fn int_test() {
        let _ = env_logger::try_init();
        assert_parse!("123" => int(123));
        assert_parse!("0" => int(0));
        assert_parse!("1" => int(1));
        assert_parse!("-1" => int(-1));
        assert_parse!("-0" => int(0));
        assert_parse!("3_000_000" => int(3_000_000));
    }

    #[test]
    fn float_test() {
        let _ = env_logger::try_init();
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
        let _ = env_logger::try_init();
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
        let _ = env_logger::try_init();
        assert_parse!("\"\"" => string(""));
        assert_parse!("\"Hello\"" => string("Hello"));
        assert_parse!("\"∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i)\"" => string("∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i)"));
    }

    #[test]
    fn unary_exprs() {
        let _ = env_logger::try_init();
        assert_parse!("- a" => fun_call("-", vec![identifier("a")]));
    }

    #[test]
    fn binary_exprs() {
        let _ = env_logger::try_init();
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
