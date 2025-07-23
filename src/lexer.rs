use crate::error::RispError;

#[derive(Debug)]
pub enum Token {
    Number(i64),
    Symbol(String),
    LeftParen,
    RightParen,
    True,
    False,
    Eof,
}

pub struct Lexer {
    input: String,
    position: usize,
    curr_char: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            curr_char: None,
        };
        lexer.curr_char = lexer.input.chars().nth(0);
        lexer
    }

    fn advance(&mut self) {
        self.position += 1;
        if self.position >= self.input.len() {
            self.curr_char = None;
        } else {
            self.curr_char = self.input.chars().nth(self.position);
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.curr_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_symbol(&mut self) -> String {
        let mut result = String::new();
        while let Some(ch) = self.curr_char {
            if ch.is_alphanumeric() || "+-*/=<>?!&".contains(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        result
    }

    fn read_number(&mut self) -> i64 {
        let mut result = String::new();
        while let Some(ch) = self.curr_char {
            if ch.is_ascii_digit() {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        result.parse::<i64>().unwrap()
    }

    pub fn next_token(&mut self) -> Result<Token, RispError> {
        self.skip_whitespace();

        match self.curr_char {
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.read_number();
                Ok(Token::Number(number))
            }
            Some(ch) if ch == '#' => {
                self.advance();
                match self.curr_char {
                    Some('t') => {
                        self.advance();
                        Ok(Token::True)
                    }
                    Some('f') => {
                        self.advance();
                        Ok(Token::False)
                    }
                    _ => Err(RispError::InvalidBoolean(
                        "Expected 't' or 'f' after '#'".to_string(),
                    )),
                }
            }
            Some(ch) if ch.is_alphabetic() || "+-*/=<>?!&".contains(ch) => {
                let symbol = self.read_symbol();
                Ok(Token::Symbol(symbol))
            }
            Some('(') => {
                self.advance();
                Ok(Token::LeftParen)
            }
            Some(')') => {
                self.advance();
                Ok(Token::RightParen)
            }
            None => Ok(Token::Eof),
            Some(ch) => Err(RispError::UnexpectedCharacter(ch)),
        }
    }
}
