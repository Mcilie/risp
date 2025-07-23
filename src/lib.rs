pub mod env;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod value;

use env::Environment;
use error::RispError;
use lexer::Lexer;
use parser::Parser;
use value::Value;

pub fn eval_expression(input: &str) -> Result<i64, RispError> {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer)?;
    let asts = parser.parse()?;
    let env = Environment::new_root(); // Create empty environment

    // Evaluate all expressions, returning the last one
    let result = eval_program(&asts, &env)?;

    // Extract integer from Value result
    match result {
        Value::Int(n) => Ok(n),
        Value::Bool(_) => Err(RispError::CannotConvertToInteger(
            "boolean result".to_string(),
        )),
        Value::Lambda { .. } => Err(RispError::CannotConvertToInteger("lambda".to_string())),
        Value::Proc(_) => Err(RispError::CannotConvertToInteger("procedure".to_string())),
    }
}

// Helper function to evaluate a program (multiple expressions)
fn eval_program(
    exprs: &[parser::Expr],
    env: &std::rc::Rc<Environment>,
) -> Result<Value, RispError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(false)); // Empty program returns false
    }

    let mut result = Value::Bool(false);
    for expr in exprs {
        result = evaluator::risp_eval(expr, env)?;
    }
    Ok(result)
}
