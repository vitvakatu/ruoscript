pub mod ast;

use self::ast::{helpers::*, Block, Expr, Module, Prototype, Statement, TopLevelStatement};
use super::lexer::{Span, Token};

use failure::Error;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Clone, Copy, PartialEq)]
enum ExpectedToken {
    Integer,
    Identifier,
    String,
    RoundLeft,
    RoundRight,
    CurlyLeft,
    Operator,
    OperatorEquals,
}

impl ::std::fmt::Debug for ExpectedToken {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        match *self {
            ExpectedToken::Integer => write!(f, "integer"),
            ExpectedToken::Identifier => write!(f, "identifier"),
            ExpectedToken::String => write!(f, "string literal"),
            ExpectedToken::RoundLeft => write!(f, "'('"),
            ExpectedToken::RoundRight => write!(f, "')'"),
            ExpectedToken::CurlyLeft => write!(f, "'{{'"),
            ExpectedToken::Operator => write!(f, "operator"),
            ExpectedToken::OperatorEquals => write!(f, "'='"),
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

    fn skip_newlines(&mut self) {
        while let Some(Span {
            inner: Token::NewLine,
            ..
        }) = self.peek_next_token()
        {
            self.next_token();
        }
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

    fn parse_string(&mut self) -> Result<Box<Expr>, Error> {
        debug!("parsing string");
        match self.next_token() {
            Some(Span {
                inner: Token::StringLiteral(s),
                ..
            }) => Ok(string(s)),
            Some(other) => Err(ParserError::Expected(ExpectedToken::String, other))?,
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
        self.skip_newlines();
        match self.peek_next_token() {
            Some(Span {
                inner: Token::Identifier(_),
                ..
            }) => self.parse_identifier(),
            Some(Span {
                inner: Token::StringLiteral(_),
                ..
            }) => self.parse_string(),
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
            let should_fold = if operator == "^" {
                token_precedence >= next_precedence
            } else {
                token_precedence < next_precedence
            };
            if should_fold {
                rhs = self.parse_binary_operator(token_precedence - 1, rhs)?;
            }

            lhs = call(operator, vec![lhs, rhs]);
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, Error> {
        // skip 'var'
        self.next_token();
        let variable_name = match self.next_token() {
            Some(Span {
                inner: Token::Identifier(ref name),
                ..
            }) => name.clone(),
            Some(other) => Err(ParserError::Expected(ExpectedToken::Identifier, other))?,
            None => Err(ParserError::Eof)?,
        };

        // skip '='
        match self.next_token() {
            Some(Span {
                inner: Token::Operator(ref op),
                ..
            })
                if op == "=" => {}
            Some(other) => Err(ParserError::Expected(ExpectedToken::OperatorEquals, other))?,
            None => Err(ParserError::Eof)?,
        }

        let init_expr = self.parse_expression()?;
        Ok(Statement::VariableDeclaration(variable_name, init_expr))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        // skip 'return'
        self.next_token();
        let expr = self.parse_expression()?;
        Ok(Statement::Return(expr))
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        debug!("parsing statement");
        match self.peek_next_token() {
            Some(Span {
                inner: Token::Var, ..
            }) => self.parse_variable_declaration(),
            Some(Span {
                inner: Token::Return,
                ..
            }) => self.parse_return_statement(),
            _ => self.parse_expression().map(Statement::Expr),
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

    fn parse_definition(&mut self) -> Result<TopLevelStatement, Error> {
        debug!("parsing definition");
        // skip 'fun'
        let _ = self.next_token();

        let proto = self.parse_prototype()?;

        self.skip_newlines();

        debug!("parsed proto: {:?}", proto);

        match self.next_token() {
            Some(Span {
                inner: Token::CurlyLeft,
                ..
            }) => {
                let mut statements = Vec::new();
                let mut statements = loop {
                    self.skip_newlines();
                    match self.peek_next_token() {
                        Some(Span {
                            inner: Token::CurlyRight,
                            ..
                        }) => break statements,
                        Some(_) => {
                            statements.push(self.parse_statement()?);
                        }
                        None => Err(ParserError::Eof)?,
                    }
                };
                self.next_token();
                // replace last statement with return
                let last_statement = statements.pop();
                let block = match last_statement {
                    Some(Statement::Expr(expr)) => Block::NonVoid(statements, expr),
                    Some(other) => {
                        statements.push(other);
                        Block::Void(statements)
                    }
                    None => Block::Void(statements),
                };
                Ok(TopLevelStatement::Function(proto, block))
            }
            Some(other) => Err(ParserError::Expected(ExpectedToken::CurlyLeft, other))?,
            None => Err(ParserError::Eof)?,
        }
    }

    fn parse_extern(&mut self) -> Result<TopLevelStatement, Error> {
        debug!("parsing extern");
        // skip 'extern'
        let _ = self.next_token();
        self.parse_prototype().map(TopLevelStatement::Prototype)
    }

    pub fn parse(&mut self) -> Result<Module, Error> {
        let mut statements = Vec::new();
        while let Some(token) = self.peek_next_token() {
            match token.inner {
                Token::Eof => return Ok(Module(statements)),
                Token::NewLine => {
                    self.next_token();
                }
                Token::Fun => {
                    let expr = self.parse_definition()?;
                    debug!("Parsed definition: {:?}", expr);
                    statements.push(expr);
                }
                Token::Extern => {
                    let statement = self.parse_extern()?;
                    debug!("Parsed extern: {:?}", statement);
                    statements.push(statement);
                }
                _ => Err(ParserError::UnexpectedTopLevel(token))?,
            }
        }
        Ok(Module(statements))
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
    use failure::Error;
    use lexer::Lexer;
    use parser::ast::{helpers::*, Block, Expr, Prototype, TopLevelStatement};

    fn parse_as_top_level(input: &str) -> Result<Module, Error> {
        let mut lexer = Lexer::new(input.char_indices());
        let tokens = lexer.get_tokens()?;
        let mut parser = Parser::new(tokens.iter());
        parser.parse()
    }

    fn parse_as_func_body(input: &str) -> Result<Module, Error> {
        let input = "fun anon_func() {".to_string() + input + "}";
        parse_as_top_level(&input)
    }

    macro_rules! assert_parse {
        ($program:expr => $block:expr) => {
            let Module(parsed) = parse_as_func_body($program).unwrap();
            assert_eq!(parsed.len(), 1);
            match parsed[0] {
                TopLevelStatement::Function(Prototype {
                    ref name,
                    ref args,
                }, ref block) => {
                    assert_eq!(name, "anon_func");
                    assert_eq!(args.len(), 0);
                    assert_eq!(*block, $block);
                }
                _ => panic!()
            }
        }
    }

    macro_rules! assert_parse_top_level {
        ($program:expr => $module:expr) => {
            assert_eq!(parse_as_top_level($program).unwrap(), $module);
        }
    }

    #[test]
    fn empty() {
        let _ = env_logger::try_init();
        assert_parse_top_level!("" => module(vec![]));
        // empty function body
        assert_parse!("" => block_void(vec![]));
    }

    #[test]
    fn integers() {
        let _ = env_logger::try_init();
        assert_parse!("123" => block_expr(int(123)));
        assert_parse!("0" => block_expr(int(0)));
        assert_parse!("1" => block_expr(int(1)));
        assert_parse!("3_000_000" => block_expr(int(3_000_000)));
        assert_parse!("3000000" => block_expr(int(3_000_000)));
    }

    #[test]
    fn strings() {
        let _ = env_logger::try_init();
        assert_parse!("''" => block_expr(string("")));
        assert_parse!("\"\"" => block_expr(string("")));
        assert_parse!("'some string'" => block_expr(string("some string")));
        assert_parse!("\"some string\"" => block_expr(string("some string")));
    }

    #[test]
    fn var_declarations() {
        let _ = env_logger::try_init();
        assert_parse!("var i = 1" => block_void(vec![
            var_declaration("i", int(1))
        ]));
        assert_parse!("var string = \"string\"" => block_void(vec![
            var_declaration("string", string("string"))
        ]));
    }

    #[test]
    fn return_statement() {
        let _ = env_logger::try_init();
        assert_parse!("return 1" => block_void(vec![ret(int(1))]));
        assert_parse!("return 1 + 1" => block_void(vec![ret(add(int(1), int(1)))]));
    }

    /*#[test]
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
    }*/

    #[test]
    fn identifiers() {
        let _ = env_logger::try_init();
        assert_parse!("a" => block_expr(variable("a")));
        assert_parse!("abc" => block_expr(variable("abc")));
        assert_parse!("a123" => block_expr(variable("a123")));
        assert_parse!("_213" => block_expr(variable("_213")));
        assert_parse!("_" => block_expr(variable("_")));
    }

    #[test]
    fn binary_exprs() {
        let _ = env_logger::try_init();
        assert_parse!("1 + 2" => block_expr(add(int(1), int(2))));
        assert_parse!("1+2" => block_expr(add(int(1), int(2))));
        assert_parse!("1 + a" => block_expr(add(int(1), variable("a"))));
        assert_parse!("a + 2" => block_expr(add(variable("a"), int(2))));
        assert_parse!("a + b" => block_expr(add(variable("a"), variable("b"))));
        assert_parse!("a+b" => block_expr(add(variable("a"), variable("b"))));
        assert_parse!("1 - 2" => block_expr(sub(int(1), int(2))));
        assert_parse!("(1 - 2)" => block_expr(sub(int(1), int(2))));
        assert_parse!("1 + 3 * 4" => block_expr(add(int(1), mul(int(3), int(4)))));
        assert_parse!("1 * 3 + 4" => block_expr(add(mul(int(1), int(3)), int(4))));
        assert_parse!("1*3+4" => block_expr(add(mul(int(1), int(3)), int(4))));
        //        assert_parse!("1 - - b" => sub(int(1), sub("-", vec![variable("b")])));
        assert_parse!("3 * (8 - 5)" => block_expr(mul(int(3), sub(int(8), int(5)))));
        assert_parse!("3 - (8 * 5)" => block_expr(sub(int(3), mul(int(8), int(5)))));
        assert_parse!("3-(8*5)" => block_expr(sub(int(3), mul(int(8), int(5)))));
        assert_parse!("3 - 8 * 5" => block_expr(sub(int(3), mul(int(8), int(5)))));
        assert_parse!("2 ^ 3 ^ 4" => block_expr(pow(int(2), pow(int(3), int(4)))));
        assert_parse!("a^b^c" => block_expr(pow(variable("a"), pow(variable("b"), variable("c")))));
    }
}

