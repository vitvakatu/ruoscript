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
pub struct Span<T> {
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
pub enum Token {
    Eof,
    Def,
    Extern,
    Identifier(String),
    Operator(String),
    Integer(i32),
    Float(i32),
}

pub struct Lexer<'a> {
    last_char: (usize, char),
    prev_char: (usize, char),
    input: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: CharIndices<'a>) -> Lexer<'a> {
        let mut input = input.peekable();
        Self {
            last_char: (0, ' '),
            prev_char: (0, ' '),
            input,
        }
    }

    fn skip_whitespaces(&mut self) {
        while let Some((_, c)) = self.input.peek().cloned() {
            if c.is_whitespace() {
                debug!("lexer: skipped one whitespace character");
                self.input.next();
                continue;
            } else {
                break;
            }
        }
    }

    fn peek_next_char(&mut self) -> bool {
        while let Some((i, c)) = self.input.peek().cloned() {
            if c.is_whitespace() {
                return false;
            }
            self.prev_char = self.last_char;
            self.last_char = (i, c);
            debug!("lexer: next char: {:?}", self.last_char);
            return true;
        }
        return false;
    }

    fn next_char(&mut self) {
        self.prev_char = self.last_char;
        self.last_char = self.input.next().unwrap();
    }

    fn prev_char(&mut self) {
        self.last_char = self.prev_char;
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
        Span::new(start, self.last_char.0 + 1, inner)
    }

    fn span_atomic(&self, inner: Token) -> Span<Token> {
        Span::new_atomic(self.last_char.0, inner)
    }

    pub fn get_token(&mut self) -> Result<Option<Span<Token>>, Error> {
        if self.at_eof() {
            debug!("lexer: Eof");
            return Ok(Some(self.span_atomic(Token::Eof)));
        }
        self.skip_whitespaces();
        self.next_char();

        // identifier: [a-zA-z_][a-zA-Z0-9_]*
        if Self::is_first_identifier_char(self.last_char.1) {
            debug!("lexer: identifier found: {:?}", self.last_char);
            return self.tokenize_identifier().map(Some);
        }

        // Integer: [0-9]+
        if Self::is_integer_char(self.last_char.1) {
            debug!("lexer: integer found: {:?}", self.last_char);
            return self.tokenize_integer().map(Some);
        }

        // Comment until end of line
        /*if self.last_char.1 == '#' {
            debug!("lexer: comment found: {:?}", self.last_char);
            return Ok(self.skip_comment());
        }*/

        // operator: [!@#$%^&*-=+?></]+
        if Self::is_operator_char(self.last_char.1) {
            debug!("lexer: operator found: {:?}", self.last_char);
            return self.tokenize_operator().map(Some);
        }

        Ok(None)
    }

    /*fn skip_comment(&mut self) -> Option<Span<Token>> {
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
    }*/

    fn tokenize_integer(&mut self) -> Result<Span<Token>, Error> {
        let mut number_str = String::new();
        let start_index = self.last_char.0;
        number_str.push(self.last_char.1);
        while self.peek_next_char() {
            if self.last_char.1.is_digit(10) {
                debug!("lexer: next digit: {:?}", self.last_char);
                number_str.push(self.last_char.1);
                self.next_char();
            } else {
                debug!("lexer: found non-digit: {:?}", self.last_char);
                self.prev_char();
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
        debug!("lexer: number parsed: {}", number);
        return Ok(self.span(start_index, Token::Integer(number)));
    }

    fn tokenize_identifier(&mut self) -> Result<Span<Token>, Error> {
        let mut identifier_str = String::new();
        let start_index = self.last_char.0;
        identifier_str.push(self.last_char.1);
        while self.peek_next_char() {
            if self.last_char.1.is_alphanumeric() || self.last_char.1 == '_' {
                debug!("lexer: next identifier character: {:?}", self.last_char);
                identifier_str.push(self.last_char.1);
                self.next_char();
            } else {
                debug!("lexer: non-identifier character found: {:?}", self.last_char);
                self.prev_char();
                break;
            }
        }
        if identifier_str == "def" {
            debug!("lexer: found def");
            return Ok(Span::new(start_index, self.last_char.0, Token::Def));
        } else if identifier_str == "extern" {
            debug!("lexer: found extern");
            return Ok(Span::new(start_index, self.last_char.0, Token::Extern));
        } else {
            debug!("lexer: parsed identifier: {}", identifier_str);
            return Ok(self.span(start_index, Token::Identifier(identifier_str)));
        }
    }

    fn tokenize_operator(&mut self) -> Result<Span<Token>, Error> {
        let mut identifier_str = String::new();
        let start_index = self.last_char.0;
        identifier_str.push(self.last_char.1);
        while self.peek_next_char() {
            if !self.last_char.1.is_alphanumeric()
                && self.last_char.1 != '_'
                && !self.last_char.1.is_whitespace()
            {
                debug!("lexer: next operator character: {:?}", self.last_char);
                self.next_char();
                identifier_str.push(self.last_char.1);
            } else {
                debug!("lexer: non-operator character found: {:?}", self.last_char);
                self.prev_char();
                break;
            }
        }
        debug!("lexer: parsed operator: {}", identifier_str);
        Ok(self.span(start_index, Token::Operator(identifier_str)))
    }

    fn is_first_identifier_char(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_identifier_char(ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    fn is_integer_char(ch: char) -> bool {
        ch.is_digit(10)
    }

    fn is_operator_char(ch: char) -> bool {
        let operator_characters = [
            '!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', ':', '<', '>', '?', '/',
        ];
        operator_characters.iter().any(|c| *c == ch)
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
        use env_logger;
        let _ = env_logger::try_init();
        debug!("Parsing {}", input);
        Lexer::new(input.char_indices())
    }

    macro_rules! assert_tokens {
        ($($input:expr => [$($tokens:expr),*]),*) => {
            $({
                let mut state = init_state($input);
                let mut tokens = vec![$($tokens),*];
                let mut parsed_tokens = state.get_tokens().unwrap();
                assert_eq!(parsed_tokens.pop().unwrap().inner, Token::Eof);
                assert_eq!(parsed_tokens, tokens);
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
            "+" => [Span::new(0, 1, op("+"))],
            "*" => [Span::new(0, 1, op("*"))]
        }
    }

    #[test]
    fn identifiers() {
        assert_tokens!{
            "a" => [Span::new(0, 1, ident("a"))],
            "abc" => [Span::new(0, 3, ident("abc"))],
            "a b" => [
                Span::new(0, 1, ident("a")),
                Span::new(2, 3, ident("b"))
                ],
            "a1" => [Span::new(0, 2, ident("a1"))]
        }
    }

    #[test]
    fn integers() {
        assert_tokens!{
            "1" => [Span::new(0, 1, int(1))],
            "1 2" => [Span::new(0, 1, int(1)), Span::new(2, 3, int(2))]
        }
    }

    #[test]
    fn expressions() {
        assert_tokens!{
            "2+2=4" => [
                Span::new(0, 1, int(2)),
                Span::new(1, 2, op("+")),
                Span::new(2, 3, int(2)),
                Span::new(3, 4, op("=")),
                Span::new(4, 5, int(4))
            ]
        }
    }
}
