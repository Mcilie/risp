#[derive(Debug)]
enum Token {
    Number(i32),
    Plus,
    Minus,
    Star,
    Slash,
    LeftParen,
    RightParen,
    Eof,
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

struct Lexer {
    input: String,
    position: usize,
    curr_char: Option<char>,
}

impl Lexer {
    fn new(input: String) -> Self {
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
            if (ch.is_whitespace()) {
                self.advance();
            } else {
                break;
            }
        }
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

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.curr_char {
            Some('+') => {
                self.advance();
                Token::Plus
            }
            Some('-') => {
                self.advance();
                Token::Minus
            }
            Some('*') => {
                self.advance();
                Token::Star
            }
            Some('/') => {
                self.advance();
                Token::Slash
            }
            Some('(') => {
                self.advance();
                Token::LeftParen
            }
            Some(')') => {
                self.advance();
                Token::RightParen
            }
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.read_number();
                Token::Number(number)
            }
            None => Token::Eof,
            Some(ch) => {
                panic!("Unexpected character: {}", ch);
            }
        }
    }
}

struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    fn token_to_binop(token: &Token) -> BinOp {
        match token {
            Token::Star => BinOp::Mul,

            Token::Plus => BinOp::Add,

            Token::Minus => BinOp::Sub,
            Token::Slash => BinOp::Div,

            _ => panic!("Expected Add or Mul! instead got {:?}", token),
        }
    }

    fn factor(&mut self) -> Expr {
        match self.current_token {
            Token::Number(n) => {
                self.advance();
                return Expr::Number(n);
            }
            Token::LeftParen => {
                self.advance();
                let expr = self.expression();
                if matches!(self.current_token, Token::RightParen) {
                    self.advance();
                    return expr;
                } else {
                    panic!("AHH PANIC YOU FORGOR A RIGHT PARENS");
                }
            }
            _ => panic!("Expected number, found {:?}", self.current_token),
        }
    }

    fn term(&mut self) -> Expr {
        let mut left = self.factor();
        while matches!(self.current_token, Token::Star | Token::Slash) {
            let operator = Self::token_to_binop(&self.current_token);
            self.advance();
            let right = self.factor();
            left = Expr::BinOp {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
            };
        }
        return left;
    }

    fn expression(&mut self) -> Expr {
        let mut left = self.term();
        while matches!(self.current_token, Token::Minus | Token::Plus) {
            let operator = Self::token_to_binop(&self.current_token);
            self.advance();
            let right = self.term();
            left = Expr::BinOp {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
            };
        }
        return left;
    }

    fn parse(&mut self) -> Expr {
        let result = self.expression();
        match self.current_token {
            Token::Eof => result,
            _ => panic!("UNEXPECTED TOKENS AFTER EXPR"),
        }
    }
}

fn risp_eval(expr: &Expr) -> i32 {
    match expr {
        Expr::Number(n) => *n,
        Expr::BinOp { left, op, right } => {
            let left_eval = risp_eval(left);
            let right_eval = risp_eval(right);
            match op {
                BinOp::Add => left_eval + right_eval,
                BinOp::Div => left_eval / right_eval,
                BinOp::Sub => left_eval - right_eval,
                BinOp::Mul => left_eval * right_eval,
            }
        }
    }
}

fn main() {
    let input = "2 + 3 * 4";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    let result = risp_eval(&ast);
    println!("{} = {}", input, result);

    let input2 = "(2 + 3) * 4";
    let lexer2 = Lexer::new(input2.to_string());
    let mut parser2 = Parser::new(lexer2);
    let ast2 = parser2.parse();
    let result2 = risp_eval(&ast2);
    println!("{} = {}", input2, result2);

    let input3 = "10 - 2 * 3 + 1";
    let lexer3 = Lexer::new(input3.to_string());
    let mut parser3 = Parser::new(lexer3);
    let ast3 = parser3.parse();
    let result3 = risp_eval(&ast3);
    println!("{} = {}", input3, result3);
}
