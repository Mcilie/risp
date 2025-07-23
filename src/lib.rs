pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod value;

use env::Environment;
use lexer::Lexer;
use parser::Parser;
use value::Value;

pub fn eval_expression(input: &str) -> i64 {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let asts = parser.parse();
    let env = Environment::new_root(); // Create empty environment

    // Use eval_program to handle multiple expressions
    let result = evaluator::risp_eval_program(&asts, &env);

    // Extract integer from Value result
    match result {
        Value::Int(n) => n,
        Value::Bool(_) => panic!(
            "Cannot convert boolean result to integer - use risp_eval directly for boolean results"
        ),
        Value::Lambda { .. } => panic!("Cannot convert lambda to integer"),
        Value::Proc(_) => panic!("Cannot convert proc to integer"),
    }
}
