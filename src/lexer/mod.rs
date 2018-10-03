mod helpers;
mod paren_counter;

use self::paren_counter::ParenCounter;

use failure::Error;
use failure::ResultExt;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Fail, Debug)]
pub enum LexerError {
    #[fail(display = "Could not parse integer: {:?}", _0)]
    CouldNotParseInteger(Span<String>),
    #[fail(display = "Could not parse float: {:?}", _0)]
    CouldNotParseFloat(Span<String>),
    #[fail(display = "Unmatched parenthesis found: {:?}", _0)]
    UnmatchedParenthesis(Span<Token>),
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
    NewLine,
    Fun,
    Var,
    Extern,
    RoundLeft,
    RoundRight,
    CurlyLeft,
    CurlyRight,
    SquareLeft,
    SquareRight,
    Comma,
    Identifier(String),
    StringLiteral(String),
    Operator(String),
    Integer(i32),
}

pub struct Lexer<'a> {
    last_char: (usize, char),
    prev_char: (usize, char),
    paren_counter: ParenCounter,
    input: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: CharIndices<'a>) -> Lexer<'a> {
        let input = input.peekable();
        Self {
            last_char: (0, ' '),
            prev_char: (0, ' '),
            paren_counter: Default::default(),
            input,
        }
    }

    fn skip_whitespaces(&mut self) {
        while let Some((_, c)) = self.input.peek().cloned() {
            if c.is_whitespace() {
                debug!("skipped one whitespace character");
                self.input.next();
                continue;
            } else {
                break;
            }
        }
    }

    // do not ignore whitespaces
    fn peek_next_char_with_whitespace(&mut self) -> bool {
        while let Some((i, c)) = self.input.peek().cloned() {
            self.prev_char = self.last_char;
            self.last_char = (i, c);
            debug!("next char: {:?}", self.last_char);
            return true;
        }
        return false;
    }

    fn peek_next_char(&mut self) -> bool {
        while let Some((i, c)) = self.input.peek().cloned() {
            if c.is_whitespace() {
                return false;
            }
            self.prev_char = self.last_char;
            self.last_char = (i, c);
            debug!("next char: {:?}", self.last_char);
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

    fn at_newline(&mut self) -> bool {
        let next = self.input.peek().unwrap();
        next.1 == '\n' || next.1 == ';'
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
            debug!("Eof");
            return Ok(Some(self.span_atomic(Token::Eof)));
        }
        if self.at_newline() {
            debug!("Newline");
            self.next_char();
            return Ok(Some(self.span_atomic(Token::NewLine)));
        }

        self.skip_whitespaces();
        self.next_char();

        // identifier: [a-zA-z_][a-zA-Z0-9_]*
        if Self::is_first_identifier_char(self.last_char.1) {
            debug!("identifier found: {:?}", self.last_char);
            return self.tokenize_identifier().map(Some);
        }

        // Integer: [0-9]+
        if Self::is_first_integer_char(self.last_char.1) {
            debug!("integer found: {:?}", self.last_char);
            return self.tokenize_integer().map(Some);
        }

        // Parenthesis: [](){}
        if Self::is_parenthesis(self.last_char.1) {
            return self.tokenize_parenthesis().map(Some);
        }

        // Comma: ,
        if self.last_char.1 == ',' {
            return Ok(Some(self.span(self.last_char.0, Token::Comma)));
        }

        // String literal: ["'].*["']
        if self.last_char.1 == '"' || self.last_char.1 == '\'' {
            return Ok(self.tokenize_string());
        }

        // Comment until end of line
        /*if self.last_char.1 == '#' {
            debug!("comment found: {:?}", self.last_char);
            return Ok(self.skip_comment());
        }*/

        // operator: any non-alphanumeric-ascii symbols, except for [;.,'"{}[]()#]
        if Self::is_operator_char(self.last_char.1) {
            debug!("operator found: {:?}", self.last_char);
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
            if Self::is_integer_char(self.last_char.1) {
                debug!("next digit: {:?}", self.last_char);
                number_str.push(self.last_char.1);
                self.next_char();
            } else {
                debug!("found non-digit: {:?}", self.last_char);
                self.prev_char();
                break;
            }
        }
        let number_str = number_str.replace("_", "");
        let number = number_str.parse().map_err(|_| {
            LexerError::CouldNotParseInteger(Span::new(
                start_index,
                self.last_char.0,
                number_str.clone(),
            ))
        })?;
        debug!("number parsed: {}", number);
        return Ok(self.span(start_index, Token::Integer(number)));
    }

    fn tokenize_string(&mut self) -> Option<Span<Token>> {
        let mut string = String::new();
        let start_index = self.last_char.0;
        while self.peek_next_char_with_whitespace() {
            self.next_char();
            if self.last_char.1 == '\'' || self.last_char.1 == '"' {
                return Some(self.span(start_index, Token::StringLiteral(string)));
            } else {
                string.push(self.last_char.1);
            }
        }
        None
    }

    fn tokenize_identifier(&mut self) -> Result<Span<Token>, Error> {
        let mut identifier_str = String::new();
        let start_index = self.last_char.0;
        identifier_str.push(self.last_char.1);
        while self.peek_next_char() {
            if Self::is_identifier_char(self.last_char.1) {
                debug!("next identifier character: {:?}", self.last_char);
                identifier_str.push(self.last_char.1);
                self.next_char();
            } else {
                debug!("non-identifier character found: {:?}", self.last_char);
                self.prev_char();
                break;
            }
        }

        match identifier_str.as_str() {
            "fun" => {
                debug!("found fun");
                Ok(Span::new(start_index, self.last_char.0, Token::Fun))
            }
            "extern" => {
                debug!("found extern");
                Ok(Span::new(start_index, self.last_char.0, Token::Extern))
            }
            "var" => {
                debug!("found var");
                Ok(Span::new(start_index, self.last_char.0, Token::Var))
            }
            _ => {
                debug!("parsed identifier: {}", identifier_str);
                Ok(self.span(start_index, Token::Identifier(identifier_str)))
            }
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
                && !Self::is_parenthesis(self.last_char.1)
            {
                debug!("next operator character: {:?}", self.last_char);
                self.next_char();
                identifier_str.push(self.last_char.1);
            } else {
                debug!("non-operator character found: {:?}", self.last_char);
                self.prev_char();
                break;
            }
        }
        debug!("parsed operator: {}", identifier_str);
        Ok(self.span(start_index, Token::Operator(identifier_str)))
    }

    fn tokenize_parenthesis(&mut self) -> Result<Span<Token>, Error> {
        let (token, is_unmatched) = match self.last_char.1 {
            '(' => (Token::RoundLeft, self.paren_counter.round_left()),
            ')' => (Token::RoundRight, self.paren_counter.round_right()),
            '{' => (Token::CurlyLeft, self.paren_counter.curly_left()),
            '}' => (Token::CurlyRight, self.paren_counter.curly_right()),
            '[' => (Token::SquareLeft, self.paren_counter.square_left()),
            ']' => (Token::SquareRight, self.paren_counter.square_right()),
            _ => unreachable!(),
        };
        if !is_unmatched {
            Err(LexerError::UnmatchedParenthesis(
                self.span(self.last_char.0, token),
            ))?
        } else {
            debug!("parsed parenthesis: {:?}", token);
            Ok(self.span(self.last_char.0, token))
        }
    }

    fn is_first_identifier_char(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn is_identifier_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }

    fn is_first_integer_char(ch: char) -> bool {
        ch.is_digit(10)
    }

    fn is_integer_char(ch: char) -> bool {
        ch.is_digit(10) || ch == '_'
    }

    fn is_parenthesis(ch: char) -> bool {
        ch == '(' || ch == ')' || ch == '{' || ch == '}' || ch == '[' || ch == ']'
    }

    fn is_operator_char(ch: char) -> bool {
        let forbidden_operator_characters = [';', '.', ',', '\'', '"', '[', ']', '{', '}', '#'];
        !ch.is_ascii_alphanumeric() && !forbidden_operator_characters.iter().any(|c| ch == *c)
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
                let mut lexer = init_state($input);
                let tokens = vec![$($tokens),*];
                let mut parsed_tokens = lexer.get_tokens().unwrap();
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
    fn operators() {
        assert_tokens!{
            "+" => [Span::new(0, 1, op("+"))],
            "*" => [Span::new(0, 1, op("*"))],
            "/" => [Span::new(0, 1, op("/"))],
            "-" => [Span::new(0, 1, op("-"))],
            "^" => [Span::new(0,1, op("^"))],
            "//" => [Span::new(0,2, op("//"))],
            "<=>" => [Span::new(0, 3, op("<=>"))],
            "<=" => [Span::new(0,2, op("<="))],
            ">=" => [Span::new(0,2, op(">="))],
            "!=" => [Span::new(0,2, op("!="))],
            "==" => [Span::new(0,2, op("=="))],
            "<" => [Span::new(0,1, op("<"))],
            ">" => [Span::new(0,1, op(">"))],
            "!" => [Span::new(0,1, op("!"))],
            "|" => [Span::new(0,1, op("|"))],
            "||" => [Span::new(0,2, op("||"))],
            "&" => [Span::new(0,1, op("&"))],
            "&&" => [Span::new(0,2, op("&&"))],
            "->" => [Span::new(0,2, op("->"))],
            "<-" => [Span::new(0,2, op("<-"))],
            "<@>" => [Span::new(0,3, op("<@>"))]
        }
    }

    #[test]
    fn identifiers() {
        assert_tokens!{
            "a" => [Span::new(0, 1, ident("a"))],
            "_" => [Span::new(0, 1, ident("_"))],
            "_prefixed" => [Span::new(0, 9, ident("_prefixed"))],
            "abc" => [Span::new(0, 3, ident("abc"))],
            "a1" => [Span::new(0, 2, ident("a1"))],
            "_123" => [Span::new(0, 4, ident("_123"))],
            "some_long_ident123" => [Span::new(0, 18, ident("some_long_ident123"))],
            "a b" => [
                Span::new(0, 1, ident("a")),
                Span::new(2, 3, ident("b"))
                ]
        }
    }

    #[test]
    fn var_declarations() {
        assert_tokens!{
            "var" => [Span::new(0, 2, var())],
            "var i = 1" => [
                Span::new(0, 2, var()),
                Span::new(4, 5, ident("i")),
                Span::new(6, 7, op("=")),
                Span::new(8, 9, int(1))
            ]
        }
    }

    #[test]
    fn strings() {
        assert_tokens!{
            "\"\"" => [Span::new(0, 2, string(""))],
            "''" => [Span::new(0, 2, string(""))],
            "\"some string\"" => [Span::new(0, 13, string("some string"))],
            "'some string'" => [Span::new(0, 13, string("some string"))]
        }
    }

    #[test]
    fn integers() {
        assert_tokens!{
            "0" => [Span::new(0, 1, int(0))],
            "1" => [Span::new(0, 1, int(1))],
            "3000000" => [Span::new(0, 7, int(3_000_000))],
            "3_000_000" => [Span::new(0, 9, int(3_000_000))],
            "1 2" => [Span::new(0, 1, int(1)), Span::new(2, 3, int(2))]
        }
    }

    #[test]
    fn multiline() {
        assert_tokens!{
            "1\n3\n\n4" => [
                Span::new(0, 1, int(1)),
                Span::new(1, 1, nl()),
                Span::new(2, 3, int(3)),
                Span::new(3, 3, nl()),
                Span::new(4, 4, nl()),
                Span::new(5, 6, int(4))
            ],
            "1;3;;4" => [
                Span::new(0, 1, int(1)),
                Span::new(1, 1, nl()),
                Span::new(2, 3, int(3)),
                Span::new(3, 3, nl()),
                Span::new(4, 4, nl()),
                Span::new(5, 6, int(4))
            ]
        }
    }

    #[test]
    fn externs() {
        assert_tokens!{
            "extern test()" => [
                Span::new(0, 5, ext()),
                Span::new(7, 11, ident("test")),
                Span::new(11, 12, round_left()),
                Span::new(12, 13, round_right())
            ],
            "extern test(a, b)" => [
                Span::new(0, 5, ext()),
                Span::new(7, 11, ident("test")),
                Span::new(11, 12, round_left()),
                Span::new(12, 13, ident("a")),
                Span::new(13, 14, comma()),
                Span::new(15, 16, ident("b")),
                Span::new(16, 17, round_right())
            ]
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
            ],
            "2 + 2 = 4" => [
                Span::new(0, 1, int(2)),
                Span::new(2, 3, op("+")),
                Span::new(4, 5, int(2)),
                Span::new(6, 7, op("=")),
                Span::new(8, 9, int(4))
            ]
        }
    }
}
