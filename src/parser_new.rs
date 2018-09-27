use failure::Error;
use failure::ResultExt;
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Fail, Debug)]
pub enum LexerError {
    #[fail(display = "Could not parse integer: {:?}", _0)]
    CouldNotParseInteger(Span<String>),
    #[fail(display = "Could not parse float: {:?}", _0)]
    CouldNotParseFloat(Span<String>),
}

#[derive(Debug, Clone, PartialEq)]
struct Span<T> {
    pub start: usize,
    pub end: usize,
    pub inner: T,
}

impl<T> Span<T> {
    pub fn new(start: usize, end: usize, inner: T) -> Self {
        Self { start, end, inner }
    }

    pub fn new_atomic(index: usize, inner: T) -> Self {
        Self {
            start: index,
            end: index,
            inner,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Eof,
    Def,
    Extern,
    Identifier(String),
    Operator(String),
    Integer(i32),
    Float(i32),
}

struct Lexer<'a> {
    last_char: (usize, char),
    input: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: CharIndices<'a>) -> Lexer<'a> {
        Self {
            last_char: (0, ' '),
            input: input.peekable(),
        }
    }

    fn skip_whitespaces(&mut self) {
        while let Some((_, c)) = self.input.peek().cloned() {
            if c.is_whitespace() {
                self.input.next();
                continue;
            } else {
                break;
            }
        }
    }

    fn next_char(&mut self) -> bool {
        while let Some((i, c)) = self.input.next() {
            if c.is_whitespace() {
                return false;
            }
            self.last_char = (i, c);
            return true;
        }
        return false;
    }

    fn at_eof(&mut self) -> bool {
        self.input.peek().is_none()
    }

    pub fn get_tokens(&mut self) -> Result<Vec<Span<Token>>, Error> {
        let mut tokens = Vec::new();
        loop {
            match self.get_token().context("Trying to read next token")? {
                Some(span) => {
                    tokens.push(span.clone());
                    if span.inner == Token::Eof {
                        break;
                    }
                }
                None => continue,
            }
        }
        Ok(tokens)
    }

    fn span(&self, start: usize, inner: Token) -> Span<Token> {
        Span::new(start, self.last_char.0, inner)
    }

    fn span_atomic(&self, inner: Token) -> Span<Token> {
        Span::new_atomic(self.last_char.0, inner)
    }

    pub fn get_token(&mut self) -> Result<Option<Span<Token>>, Error> {
        self.skip_whitespaces();
        if self.at_eof() {
            return Ok(Some(self.span_atomic(Token::Eof)));
        }
        self.next_char();

        // identifier: [a-zA-z_][a-zA-Z0-9_]*
        if self.last_char.1.is_alphabetic() || self.last_char.1 == '_' {
            return self.tokenize_identifier().map(Some);
        }

        // Integer: [0-9]+
        if self.last_char.1.is_digit(10) {
            return self.tokenize_integer().map(Some);
        }

        // Comment until end of line
        if self.last_char.1 == '#' {
            return Ok(self.skip_comment());
        }

        Ok(None)
    }

    fn skip_comment(&mut self) -> Option<Span<Token>> {
        while self.next_char() {
            if self.last_char.1 != '\n' && self.last_char.1 != '\r' {
                continue;
            }
            if self.at_eof() {
                return Some(self.span_atomic(Token::Eof));
            } else {
                return None;
            }
        }
        None
    }

    fn tokenize_integer(&mut self) -> Result<Span<Token>, Error> {
        let mut number_str = String::new();
        let start_index = self.last_char.0;
        number_str.push(self.last_char.1);
        while self.next_char() {
            if self.last_char.1.is_digit(10) {
                number_str.push(self.last_char.1);
            } else {
                break;
            }
        }
        let number = number_str.parse().map_err(|_| {
            LexerError::CouldNotParseInteger(Span::new(
                start_index,
                self.last_char.0,
                number_str.clone(),
            ))
        })?;
        return Ok(self.span(start_index, Token::Integer(number)));
    }

    fn tokenize_identifier(&mut self) -> Result<Span<Token>, Error> {
        let mut identifier_str = String::new();
        let start_index = self.last_char.0;
        identifier_str.push(self.last_char.1);
        while self.next_char() {
            if self.last_char.1.is_alphanumeric() || self.last_char.1 == '_' {
                identifier_str.push(self.last_char.1);
            } else {
                break;
            }
        }
        if identifier_str == "def" {
            return Ok(Span::new(start_index, self.last_char.0, Token::Def));
        } else if identifier_str == "extern" {
            return Ok(Span::new(start_index, self.last_char.0, Token::Extern));
        } else {
            return Ok(self.span(start_index, Token::Identifier(identifier_str)));
        }
    }
}

pub mod helpers {
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
}

#[cfg(test)]
mod tests {
    use super::helpers::*;
    use super::*;

    fn init_state(input: &str) -> Lexer {
        Lexer::new(input.char_indices())
    }

    macro_rules! assert_tokens {
        ($($input:expr => [$($tokens:expr),*]),*) => {
            $({
                let mut state = init_state($input);
                let mut tokens = vec![$($tokens),*];
                tokens.push(Token::Eof);
                assert_eq!(state.get_tokens().unwrap(), tokens);
            })*
        }
    }

    #[test]
    fn empty() {
        assert_tokens!{
            "" => []
        }
    }

    #[test]
    fn single_char() {
        assert_tokens!{
            "+" => [Token::Other('+')],
            "*" => [Token::Other('*')]
        }
    }

    #[test]
    fn identifiers() {
        assert_tokens!{
            "a" => [Token::Identifier("a".into())],
            "abc" => [Token::Identifier("abc".into())],
            "a b" => [
                Token::Identifier("a".into()),
                Token::Identifier("b".into())
                ],
            "a1" => [Token::Identifier("a1".into())]
        }
    }

    #[test]
    fn numbers() {
        assert_tokens!{
            "1" => [Token::Number(1.0)],
            "1.0" => [Token::Number(1.0)],
            "3.14 5.26" => [Token::Number(3.14), Token::Number(5.26)]
        }
    }

    #[test]
    fn expressions() {
        assert_tokens!{
            "2+2=4" => [
                Token::Number(2.0),
                Token::Other('+'),
                Token::Number(2.0),
                Token::Other('='),
                Token::Number(4.0)
            ]
        }
    }
}
