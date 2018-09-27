use std::str::CharIndices;
use std::iter::Peekable;

#[derive(Fail, Debug)]
pub enum LexerError {

}

#[derive(Debug, Clone, PartialEq)]
struct Span<T> {
    pub start: usize,
    pub end: usize,
    pub inner: T,
}

impl<T> Span<T> {
    pub fn new(start: usize, end: usize, inner: T) -> Self {
        Self {
            start,
            end,
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
                continue
            } else {
                break
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

    pub fn get_tokens(&mut self) -> Vec<Span<Token>> {
        let mut tokens = Vec::new();
        loop {
            match self.get_token() {
                Some(token) => {
                    tokens.push(token.clone());
                    if token == Token::Eof {
                        break;
                    }
                }
                None => continue
            }
        }
        tokens
    }

    pub fn get_token(&mut self) -> Result<Option<Span<Token>>, LexerError> {
        self.skip_whitespaces();
        if self.at_eof() {
            return Ok(Some(Span::new(self.last_char.0, self.last_char.0, Token::Eof)));
        }
        self.next_char();

        // identifier: [a-zA-z_][a-zA-Z0-9_]*
        if self.last_char.is_alphabetic() || self.last_char == '_' {
            let mut identifier_str = String::new();
            let start_index = self.last_char.0;
            identifier_str.push(self.last_char.1);
            while self.next_char() {
                if self.last_char.is_alphanumeric() || self.last_char == '_' {
                    identifier_str.push(self.last_char.1);
                } else {
                    break;
                }
            }
            if identifier_str == "def" {
                return Ok(Some(Span::new(start_index, self.last_char.0, Token::Def)));
            } else if identifier_str == "extern" {
                return Ok(Some(Span::new(start_index, self.last_char.0, Token::Extern)));
            } else {
                return Ok(Some(Span::new(start_index, self.last_char.0, Token::Identifier(identifier_str))));
            }
        }

        // Integer: [0-9]+
        if self.last_char.is_digit(10) {
            let mut number_str = String::new();
            let start_index = self.last_char.0;
            number_str.push(self.last_char.1);
            while self.next_char() {
                if self.last_char.is_digit(10) {
                    number_str.push(self.last_char.1);
                } else {

                }
            }
            let number = number_str.parse().expect("Could not parse number");
            return Some(Span::new(start_index, self.last_char.0, Token::Number(number));
        }

        // Comment until end of line
        if self.last_char == '#' {
            while self.next_char() {
                if self.last_char != '\n' && self.last_char != '\r' {
                    continue
                }
                if self.at_eof() {
                    return Some(Span::new(self.last_char.0, self.last_char.0, Token::Eof));
                } else {
                    return None;
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
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
                assert_eq!(state.get_tokens(), tokens);
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
