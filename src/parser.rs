use crate::error::RispError;
use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Symbol(String),  // +, -, foo, bar
    List(Vec<Expr>), // (+ 1 2), (foo bar baz)
    Boolean(bool),
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Result<Self, RispError> {
        let current_token = lexer.next_token()?;
        Ok(Parser {
            lexer,
            current_token,
        })
    }

    fn advance(&mut self) -> Result<(), RispError> {
        self.current_token = self.lexer.next_token()?;
        Ok(())
    }

    pub fn parse_expr(&mut self) -> Result<Expr, RispError> {
        match &self.current_token {
            Token::Number(n) => {
                let num = *n;
                self.advance()?;
                Ok(Expr::Number(num))
            }
            Token::True => {
                self.advance()?;
                Ok(Expr::Boolean(true))
            }
            Token::False => {
                self.advance()?;
                Ok(Expr::Boolean(false))
            }
            Token::Symbol(s) => {
                let symbol = s.clone();
                self.advance()?;
                Ok(Expr::Symbol(symbol))
            }
            Token::LeftParen => {
                self.advance()?; // Skip '('
                let mut elements = Vec::new();

                // Read list elements until ')'
                while !matches!(self.current_token, Token::RightParen | Token::Eof) {
                    elements.push(self.parse_expr()?); // Recursive!
                }

                if matches!(self.current_token, Token::RightParen) {
                    self.advance()?; // Skip ')'
                    Ok(Expr::List(elements))
                } else {
                    Err(RispError::MissingClosingParen)
                }
            }
            Token::Eof => Err(RispError::UnexpectedEndOfInput),
            _ => Err(RispError::UnexpectedToken(format!(
                "{:?}",
                self.current_token
            ))),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, RispError> {
        let mut expressions = Vec::new();

        while !matches!(self.current_token, Token::Eof) {
            expressions.push(self.parse_expr()?);
        }

        Ok(expressions)
    }
}
