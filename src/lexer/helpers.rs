#![allow(unused)]
use super::Token;

pub fn ident(i: &str) -> Token {
    Token::Identifier(i.to_string())
}

pub fn op(i: &str) -> Token {
    Token::Operator(i.to_string())
}

pub fn int(n: i32) -> Token {
    Token::Integer(n)
}

pub fn string(s: &str) -> Token {
    Token::StringLiteral(s.to_string())
}

pub fn nl() -> Token {
    Token::NewLine
}

pub fn ext() -> Token {
    Token::Extern
}

pub fn fun() -> Token {
    Token::Fun
}

pub fn round_left() -> Token {
    Token::RoundLeft
}

pub fn round_right() -> Token {
    Token::RoundRight
}

pub fn curly_left() -> Token {
    Token::CurlyLeft
}

pub fn curly_right() -> Token {
    Token::CurlyRight
}

pub fn square_left() -> Token {
    Token::SquareLeft
}

pub fn square_right() -> Token {
    Token::SquareRight
}

pub fn comma() -> Token {
    Token::Comma
}
