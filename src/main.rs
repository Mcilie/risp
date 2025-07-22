mod env;
mod evaluator;
mod lexer;
mod parser;
mod value;

use env::Environment;
use evaluator::risp_eval;
use lexer::Lexer;
use parser::Parser;
use risp::eval_expression;
use std::rc::Rc;
use value::Value;

fn test_expression(input: &str, env: &Rc<Environment>) {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    let result = risp_eval(&ast, env);
    println!("{} = {}", input, result);
}

fn main() {
    println!("=== Risp - A Lisp Interpreter ===\n");

    let env = Environment::new_root();

    println!("1. Variable Definition:");
    test_expression("(define x 10)", &env);
    test_expression("(define y 5)", &env);
    test_expression("x", &env);
    test_expression("y", &env);

    println!("\n2. Boolean Literals:");
    test_expression("#t", &env);
    test_expression("#f", &env);

    println!("\n3. Logical Operations:");
    test_expression("(and #t #t)", &env);
    test_expression("(and #t #f)", &env);
    test_expression("(or #f #t)", &env);
    test_expression("(or #f #f)", &env);
    test_expression("(not #t)", &env);
    test_expression("(not #f)", &env);

    println!("\n4. Comparisons:");
    test_expression("(= 5 5)", &env);
    test_expression("(= 3 7)", &env);
    test_expression("(< x y)", &env); // x=10, y=5, so false
    test_expression("(> x y)", &env); // x=10, y=5, so true

    println!("\n5. If Statements:");
    test_expression("(if #t 42 99)", &env);
    test_expression("(if #f 42 99)", &env);
    test_expression("(if (> x 7) 100 200)", &env); // x=10 > 7, so 100
    test_expression("(if (< x 7) 300)", &env); // 2-arg form, x=10 not < 7, so #f

    println!("\n6. Complex Expressions:");
    test_expression("(define age 25)", &env);
    test_expression("(if (and (> age 18) (< age 65)) 1 0)", &env); // working age = 1
    test_expression("(+ x (if (> y 3) y 0))", &env); // 10 + (if 5>3 then 5 else 0) = 15

    println!("\n7. Arithmetic (still works!):");
    // Using the convenience function for simple arithmetic (no booleans)
    let arithmetic_examples = vec!["(+ 1 2 3 4)", "(- 10 3 2)", "(* 2 3 4)", "(/ 24 2 3)"];

    for example in arithmetic_examples {
        let result = eval_expression(example);
        println!("{} = {}", example, result);
    }
}
