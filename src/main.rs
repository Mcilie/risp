mod env;
mod error;
mod evaluator;
mod lexer;
mod parser;
mod value;

use env::Environment;
use error::RispError;
use evaluator::risp_eval;
use lexer::Lexer;
use parser::Parser;
use std::env::args;
use std::fs;
use std::process;
use std::rc::Rc;

fn eval_program(input: &str, env: &Rc<Environment>) -> Result<value::Value, RispError> {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer)?;
    let asts = parser.parse()?;

    if asts.is_empty() {
        return Ok(value::Value::Bool(false));
    }

    let mut result = value::Value::Bool(false);
    for expr in &asts {
        result = risp_eval(expr, env)?;
    }
    Ok(result)
}

fn main() {
    let args: Vec<String> = args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <filename.risp>", args[0]);
        eprintln!("Example: cargo run sample.risp");
        process::exit(1);
    }

    let filename = &args[1];

    // Read the file
    let program = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    // Create environment and execute program
    let env = Environment::new_root();

    match eval_program(&program, &env) {
        Ok(result) => {
            // Only print result if it's not the default false from empty program
            match result {
                value::Value::Bool(false) => {} // Don't print default empty result
                _ => println!("Program result: {}", result),
            }
        }
        Err(e) => {
            eprintln!("Risp Error: {}", e);
            process::exit(1);
        }
    }
}
