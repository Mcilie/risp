use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Symbol(String),  // +, -, foo, bar
    List(Vec<Expr>), // (+ 1 2), (foo bar baz)
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Expr {
        match &self.current_token {
            Token::Number(n) => {
                let num = *n;
                self.advance();
                Expr::Number(num)
            }
            Token::Symbol(s) => {
                let symbol = s.clone();
                self.advance();
                Expr::Symbol(symbol)
            }
            Token::LeftParen => {
                self.advance(); // Skip '('
                let mut elements = Vec::new();

                // Read list elements until ')'
                while !matches!(self.current_token, Token::RightParen | Token::Eof) {
                    elements.push(self.parse()); // Recursive!
                }

                if matches!(self.current_token, Token::RightParen) {
                    self.advance(); // Skip ')'
                    Expr::List(elements)
                } else {
                    panic!("Expected closing parenthesis");
                }
            }
            Token::Eof => panic!("Unexpected end of input"),
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }
}
