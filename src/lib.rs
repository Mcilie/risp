pub mod evaluator;
pub mod lexer;
pub mod parser;

use evaluator::risp_eval;
use lexer::Lexer;
use parser::Parser;

pub fn eval_expression(input: &str) -> i32 {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    risp_eval(&ast)
}
