mod evaluator;
mod lexer;
mod parser;

use evaluator::risp_eval;
use lexer::Lexer;
use parser::Parser;
use risp::eval_expression;

fn main() {
    println!("=== Risp - A Lisp Interpreter ===\n");

    // Show direct usage of components (eliminates dead code warnings)
    println!("Direct component usage:");
    let input = "(+ 2 (* 3 4))";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    let result = risp_eval(&ast);
    println!("{} = {}", input, result);

    println!("\nMore examples:");
    // Using the convenience function
    let examples = vec![
        "(+ 1 2 3 4)",
        "(* 2 (+ 3 4))",
        "(- 10 (/ 8 2))",
        "(- (- 5))", // Double negation
        "(+ (* 2 3) (- 8 3))",
    ];

    for expr in examples {
        println!("{} = {}", expr, eval_expression(expr));
    }
}
