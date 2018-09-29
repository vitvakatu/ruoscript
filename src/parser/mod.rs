pub mod ast;

use self::ast::{helpers::*, Expr};
use super::lexer::{Lexer, Token};

use codegen::{Codegen, Context};

use std::iter::Peekable;
use std::slice::Iter;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Iter<'a, Token>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next().cloned()
    }

    fn peek_next_token(&mut self) -> Option<Token> {
        self.tokens.peek().cloned().cloned()
    }

    fn parse_integer(&mut self) -> Box<Expr> {
        debug!("parser: parsing integer");
        let number = if let Some(Token::Integer(i)) = self.next_token() {
            i
        } else {
            unimplemented!()
        };
        debug!("parser: parsed integer: {}", number);
        int(number)
    }

    fn parse_paren_expression(&mut self) -> Box<Expr> {
        debug!("parser: parsing paren expression");
        // skip '('
        let _ = self.next_token();
        let expr = self.parse_expression();
        match self.peek_next_token() {
            Some(Token::RoundRight) => {
                // skip ')'
                let _ = self.next_token();
            }
            _ => unimplemented!(),
        }
        debug!("parser: parsed paren expression: {:?}", expr);
        expr
    }

    fn parse_identifier(&mut self) -> Box<Expr> {
        debug!("parser: parsing identifier");
        let identifier = match self.next_token() {
            Some(Token::Identifier(identifier)) => identifier,
            _ => unimplemented!(),
        };
        match self.peek_next_token() {
            Some(Token::RoundLeft) => {
                debug!("parser: found function call");
                // skip '('
                let _ = self.next_token();
                // parse arguments
                let mut args = Vec::new();
                // empty arguments list
                if let Some(Token::RoundRight) = self.peek_next_token() {
                    let _ = self.next_token();
                    return call(identifier, args);
                }
                loop {
                    let arg = self.parse_expression();
                    args.push(arg);

                    match self.peek_next_token() {
                        Some(Token::RoundRight) => {
                            let _ = self.next_token();
                            break;
                        }
                        Some(Token::Comma) => {
                            let _ = self.next_token();
                        }
                        _ => panic!(),
                    }
                }
                debug!("parser: parsed function call: {}, {:?}", identifier, args);
                call(identifier, args)
            }
            _ => return variable(identifier),
        }
    }

    fn parse_primary(&mut self) -> Box<Expr> {
        debug!("parser: parsing primary");
        match self.peek_next_token() {
            Some(Token::Identifier(_)) => self.parse_identifier(),
            Some(Token::Integer(_)) => self.parse_integer(),
            Some(Token::RoundLeft) => self.parse_paren_expression(),
            _ => panic!(),
        }
    }

    fn parse_binary_operator(&mut self, expr_precedence: u32, mut lhs: Box<Expr>) -> Box<Expr> {
        debug!("parser: parsing binary operator");
        loop {
            debug!("parser: lhs = {:?}", lhs);
            let token_precedence = self.get_token_precedence();

            if token_precedence <= expr_precedence {
                debug!("parser: parsed binary expresion: {:?}", lhs);
                return lhs;
            }

            let operator = match self.next_token() {
                Some(Token::Operator(op)) => op,
                Some(Token::Eof) | Some(Token::NewLine) => return lhs,
                _ => unreachable!(),
            };

            debug!("parser: operator = {}", operator);
            let mut rhs = self.parse_primary();
            debug!("parser: rhs = {:?}", rhs);

            let next_precedence = self.get_token_precedence();
            if token_precedence < next_precedence {
                rhs = self.parse_binary_operator(token_precedence + 1, rhs);
            }

            lhs = call(operator, vec![lhs, rhs]);
        }
    }

    fn parse_expression(&mut self) -> Box<Expr> {
        debug!("parser: parsing expression");
        let lhs = self.parse_primary();
        self.parse_binary_operator(0, lhs)
    }

    fn parse_prototype(&mut self) -> Box<Expr> {
        debug!("parser: parsing prototype");
        let identifier = match self.next_token() {
            Some(Token::Identifier(s)) => s,
            _ => panic!(),
        };

        match self.next_token() {
            Some(Token::RoundLeft) => {
                // parse arguments
                let mut args = Vec::new();
                // empty arguments list
                if let Some(Token::RoundRight) = self.peek_next_token() {
                    let _ = self.next_token();
                    return prototype(identifier, args);
                }
                loop {
                    let arg = if let Some(Token::Identifier(s)) = self.next_token() {
                        s
                    } else {
                        panic!()
                    };
                    args.push(arg);

                    match self.peek_next_token() {
                        Some(Token::RoundRight) => {
                            let _ = self.next_token();
                            break;
                        }
                        Some(Token::Comma) => {
                            let _ = self.next_token();
                        }
                        _ => panic!(),
                    }
                }
                prototype(identifier, args)
            }

            _ => panic!(),
        }
    }

    fn parse_definition(&mut self) -> Box<Expr> {
        debug!("parser: parsing definition");
        // skip 'def'
        let _ = self.next_token();

        let proto = if let Expr::Prototype(proto) = *self.parse_prototype() {
            proto
        } else {
            unreachable!()
        };

        debug!("parser: parsed proto: {:?}", proto);

        match self.next_token() {
            Some(Token::CurlyLeft) => {
                let body = self.parse_expression();
                let res = function(proto, body);
                if let Some(Token::CurlyRight) = self.next_token() {
                    res
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        }
    }

    fn parse_top_level(&mut self) -> Box<Expr> {
        debug!("parser: parsing top level");
        let expr = self.parse_expression();
        let proto = ast::Prototype {
            name: "__anon_expr".into(),
            args: vec![],
        };
        function(proto, expr)
    }

    fn parse_extern(&mut self) -> Box<Expr> {
        debug!("parser: parsing extern");
        // skip 'extern'
        let _ = self.next_token();
        self.parse_prototype()
    }

    pub fn parse(&mut self) {
        while let Some(token) = self.peek_next_token() {
            match token {
                Token::Eof => return,
                Token::NewLine => {
                    self.next_token();
                }
                Token::Def => {
                    let expr = self.parse_definition();
                    println!("{:?}", expr);
                    Self::codegen(expr);
                    break;
                }
                Token::Extern => {
                    let expr = self.parse_extern();
                    println!("{:?}", expr);
                    Self::codegen(expr);
                    break;
                }
                _ => {
                    let expr = self.parse_top_level();
                    println!("{:?}", expr);
                    Self::codegen(expr);
                    break;
                }
            }
        }
    }

    fn codegen(expr: Box<Expr>) {
        let mut context = Context::new();
        let value = expr.codegen(&mut context);
        let string = unsafe {
            ::std::ffi::CStr::from_ptr(::llvm_sys::core::LLVMPrintValueToString(value))
        };
        println!("{}", string.to_str().unwrap());
    }

    fn get_token_precedence(&mut self) -> u32 {
        match self.peek_next_token() {
            Some(Token::Operator(s)) => match s.as_str() {
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
            },
            _ => 0,
        }
    }
}
