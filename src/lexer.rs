#[derive(Debug)]
pub enum Token {
    Number(i32),
    Symbol(String),
    LeftParen,
    RightParen,
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

    fn read_number(&mut self) -> i32 {
        let mut result = String::new();
        while let Some(ch) = self.curr_char {
            if ch.is_ascii_digit() {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        result.parse::<i32>().unwrap()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.curr_char {
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.read_number();
                Token::Number(number)
            }
            Some(ch) if ch.is_alphabetic() || "+-*/=<>?!&".contains(ch) => {
                let symbol = self.read_symbol();
                Token::Symbol(symbol)
            }
            Some('(') => {
                self.advance();
                Token::LeftParen
            }
            Some(')') => {
                self.advance();
                Token::RightParen
            }
            None => Token::Eof,
            Some(ch) => {
                panic!("Unexpected character: {}", ch);
            }
        }
    }
}
