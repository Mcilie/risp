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
    let asts = parser.parse();

    // If it's a single expression, print with = format
    // If it's multiple expressions, evaluate all but only show the last result
    if asts.len() == 1 {
        let result = risp_eval(&asts[0], env);
        println!("{} = {}", input, result);
    } else {
        // Evaluate all expressions in sequence
        let result = evaluator::risp_eval_program(&asts, env);
        println!("{}\n=> {}", input, result);
    }
}

fn main() {
    println!("=== Risp - A Lisp Interpreter ===\n");

    let env = Environment::new_root();

    // Parse and evaluate each expression separately
    test_expression(
        "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))",
        &env,
    );
    test_expression("(fact 5)", &env);

    // println!("1. Variable Definition:");
    // test_expression("(define x 10)", &env);
    // test_expression("(define y 5)", &env);
    // test_expression("x", &env);
    // test_expression("y", &env);

    // println!("\n2. Boolean Literals:");
    // test_expression("#t", &env);
    // test_expression("#f", &env);

    // println!("\n3. Logical Operations:");
    // test_expression("(and #t #t)", &env);
    // test_expression("(and #t #f)", &env);
    // test_expression("(or #f #t)", &env);
    // test_expression("(or #f #f)", &env);
    // test_expression("(not #t)", &env);
    // test_expression("(not #f)", &env);

    // println!("\n4. Comparisons:");
    // test_expression("(= 5 5)", &env);
    // test_expression("(= 3 7)", &env);
    // test_expression("(< x y)", &env); // x=10, y=5, so false
    // test_expression("(> x y)", &env); // x=10, y=5, so true

    // println!("\n5. If Statements:");
    // test_expression("(if #t 42 99)", &env);
    // test_expression("(if #f 42 99)", &env);
    // test_expression("(if (> x 7) 100 200)", &env); // x=10 > 7, so 100
    // test_expression("(if (< x 7) 300)", &env); // 2-arg form, x=10 not < 7, so #f

    // println!("\n6. Complex Expressions:");
    // test_expression("(define age 25)", &env);
    // test_expression("(if (and (> age 18) (< age 65)) 1 0)", &env); // working age = 1
    // test_expression("(+ x (if (> y 3) y 0))", &env); // 10 + (if 5>3 then 5 else 0) = 15

    // println!("\n7. Function Definition Sugar:");
    // test_expression("(define (square n) (* n n))", &env);
    // test_expression("(square 5)", &env); // Should be 25
    // test_expression("(define (add2 a b) (+ a b))", &env);
    // test_expression("(add2 10 20)", &env); // Should be 30
    // test_expression("(define (triple x) (* 3 x))", &env);
    // test_expression("(triple 7)", &env); // Should be 21

    // println!("\n8. Lambda Expressions:");
    // test_expression("((lambda (x) (* x 2)) 6)", &env); // Should be 12
    // test_expression("(define doubler (lambda (n) (* n 2)))", &env);
    // test_expression("(doubler 8)", &env); // Should be 16

    // println!("\n9. Arithmetic (still works!):");
    // // Using the convenience function for simple arithmetic (no booleans)
    // let arithmetic_examples = vec!["(+ 1 2 3 4)", "(- 10 3 2)", "(* 2 3 4)", "(/ 24 2 3)"];

    // for example in arithmetic_examples {
    //     let result = eval_expression(example);
    //     println!("{} = {}", example, result);
    // }

    println!("\n=== Testing Multi-Form Parsing ===");
    test_expression("(define x 10) (define y 20) (+ x y)", &env);
    test_expression(
        "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1))))) (sum 5)",
        &env,
    );
}
