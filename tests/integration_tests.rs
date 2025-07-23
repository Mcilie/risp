use risp::eval_expression;
use risp::{env::Environment, evaluator, lexer::Lexer, parser::Parser, value::Value};
use std::rc::Rc;

// Helper function to test expressions that might return booleans
fn eval_full(input: &str) -> Value {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let asts = parser.parse();
    let env = Environment::new_root();

    // Use risp_eval_program to handle both single and multiple expressions
    evaluator::risp_eval_program(&asts, &env)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_numbers() {
        assert_eq!(eval_expression("42"), 42);
        assert_eq!(eval_expression("0"), 0);
        assert_eq!(eval_expression("123"), 123);
    }

    #[test]
    fn test_basic_addition() {
        assert_eq!(eval_expression("(+ 2 3)"), 5);
        assert_eq!(eval_expression("(+ 10 5)"), 15);
        assert_eq!(eval_expression("(+ 1 2 3)"), 6);
    }

    #[test]
    fn test_basic_subtraction() {
        assert_eq!(eval_expression("(- 5 3)"), 2);
        assert_eq!(eval_expression("(- 10 5)"), 5);
        assert_eq!(eval_expression("(- 10 3 2)"), 5); // 10 - 3 - 2 = 5
    }

    #[test]
    fn test_basic_multiplication() {
        assert_eq!(eval_expression("(* 3 4)"), 12);
        assert_eq!(eval_expression("(* 2 5)"), 10);
        assert_eq!(eval_expression("(* 2 3 4)"), 24);
    }

    #[test]
    fn test_basic_division() {
        assert_eq!(eval_expression("(/ 8 2)"), 4);
        assert_eq!(eval_expression("(/ 15 3)"), 5);
        assert_eq!(eval_expression("(/ 24 3)"), 8); // Can't chain division easily in Lisp
    }

    #[test]
    fn test_unary_operators() {
        // Basic unary negation
        assert_eq!(eval_expression("(- 5)"), -5);
        assert_eq!(eval_expression("(- 0)"), 0);
        assert_eq!(eval_expression("(- 42)"), -42);

        // Unary with nested expressions
        assert_eq!(eval_expression("(- (+ 2 3))"), -5);
        assert_eq!(eval_expression("(- (- 10 4))"), -6);
        assert_eq!(eval_expression("(- (* 3 4))"), -12);

        // Double negation
        assert_eq!(eval_expression("(- (- 5))"), 5);
        assert_eq!(eval_expression("(- (- (- 3)))"), -3);

        // Unary in binary expressions
        assert_eq!(eval_expression("(+ 5 (- 3))"), 2);
        assert_eq!(eval_expression("(+ (- 2) 7)"), 5);
        assert_eq!(eval_expression("(- 10 (- 3))"), 13);
        assert_eq!(eval_expression("(- (- 5) (- 2))"), -3);

        // Unary with multiplication/division
        assert_eq!(eval_expression("(* (- 2) 3)"), -6);
        assert_eq!(eval_expression("(* 2 (- 3))"), -6);
        assert_eq!(eval_expression("(/ (- 8) 2)"), -4);
        assert_eq!(eval_expression("(/ 8 (- 2))"), -4);

        // Complex expressions with unary
        assert_eq!(eval_expression("(+ (- 2) (* 3 4))"), 10);
        assert_eq!(eval_expression("(* (- (+ 2 3)) 4)"), -20);
        assert_eq!(eval_expression("(+ (* (- 5) (- 3)) 1)"), 16);
    }

    #[test]
    fn test_nested_expressions() {
        // Test nested S-expressions (no precedence - explicit grouping)
        assert_eq!(eval_expression("(+ 2 (* 3 4))"), 14);
        assert_eq!(eval_expression("(- 10 (* 2 3))"), 4);
        assert_eq!(eval_expression("(+ (* 2 3) 4)"), 10);
        assert_eq!(eval_expression("(+ (/ 20 4) 1)"), 6);
    }

    #[test]
    fn test_parentheses_grouping() {
        // All grouping is now explicit with S-expressions
        assert_eq!(eval_expression("(* (+ 2 3) 4)"), 20);
        assert_eq!(eval_expression("(* 2 (+ 3 4))"), 14);
        assert_eq!(eval_expression("(/ (- 10 2) (+ 3 1))"), 2);
        assert_eq!(eval_expression("(* (+ 2 3) 4)"), 20);
    }

    #[test]
    fn test_complex_expressions() {
        assert_eq!(eval_expression("(- (+ 2 (* 3 4)) 1)"), 13);
        assert_eq!(eval_expression("(* (+ 2 3) (- 4 1))"), 15);
        assert_eq!(eval_expression("(+ (- 10 (* 2 3)) 1)"), 5);
        assert_eq!(eval_expression("(+ (/ 20 (+ 2 2)) (* 3 2))"), 11);
    }

    #[test]
    fn test_whitespace_handling() {
        assert_eq!(eval_expression("(+ 2 3)"), 5);
        assert_eq!(eval_expression("  ( +   2   3  )  "), 5);
        assert_eq!(eval_expression("(* 2 (+ 3 4))"), 14);
        assert_eq!(eval_expression("\t(\n+\r2\t3)\n"), 5);
    }

    #[test]
    fn test_variable_arity() {
        // Test Lisp's variable arity functions
        assert_eq!(eval_expression("(+ 1 2 3 4)"), 10);
        assert_eq!(eval_expression("(+ 2 3 4)"), 9);
        assert_eq!(eval_expression("(* 2 3 4)"), 24);
        assert_eq!(eval_expression("(* 1 1 1 1)"), 1);
    }

    #[test]
    fn test_deeply_nested() {
        assert_eq!(eval_expression("(* (+ 2 3) (+ 4 1))"), 25);
        assert_eq!(eval_expression("(+ 1 2)"), 3);
        assert_eq!(eval_expression("(* 2 (+ (+ 3 4) (- 5 2)))"), 20);
    }

    #[test]
    fn test_edge_cases() {
        assert_eq!(eval_expression("(* 0 100)"), 0);
        assert_eq!(eval_expression("(+ 100 0)"), 100);
        assert_eq!(eval_expression("(* 1 1 1 1)"), 1);
        assert_eq!(eval_expression("0"), 0);
    }
}

#[cfg(test)]
mod boolean_tests {
    use super::*;

    #[test]
    fn test_boolean_literals() {
        assert_eq!(eval_full("#t"), Value::Bool(true));
        assert_eq!(eval_full("#f"), Value::Bool(false));
    }

    #[test]
    fn test_logical_operations() {
        // AND operation
        assert_eq!(eval_full("(and #t #t)"), Value::Bool(true));
        assert_eq!(eval_full("(and #t #f)"), Value::Bool(false));
        assert_eq!(eval_full("(and #f #t)"), Value::Bool(false));
        assert_eq!(eval_full("(and #f #f)"), Value::Bool(false));

        // OR operation
        assert_eq!(eval_full("(or #t #t)"), Value::Bool(true));
        assert_eq!(eval_full("(or #t #f)"), Value::Bool(true));
        assert_eq!(eval_full("(or #f #t)"), Value::Bool(true));
        assert_eq!(eval_full("(or #f #f)"), Value::Bool(false));

        // NOT operation
        assert_eq!(eval_full("(not #t)"), Value::Bool(false));
        assert_eq!(eval_full("(not #f)"), Value::Bool(true));
    }

    #[test]
    fn test_comparison_operations() {
        // Equality
        assert_eq!(eval_full("(= 5 5)"), Value::Bool(true));
        assert_eq!(eval_full("(= 3 7)"), Value::Bool(false));
        assert_eq!(eval_full("(= 1 1 1)"), Value::Bool(true));
        assert_eq!(eval_full("(= 1 2 1)"), Value::Bool(false));

        // Less than
        assert_eq!(eval_full("(< 3 5)"), Value::Bool(true));
        assert_eq!(eval_full("(< 5 3)"), Value::Bool(false));
        assert_eq!(eval_full("(< 5 5)"), Value::Bool(false));

        // Greater than
        assert_eq!(eval_full("(> 5 3)"), Value::Bool(true));
        assert_eq!(eval_full("(> 3 5)"), Value::Bool(false));
        assert_eq!(eval_full("(> 5 5)"), Value::Bool(false));
    }

    #[test]
    fn test_complex_boolean_expressions() {
        assert_eq!(eval_full("(and (> 5 3) (< 2 4))"), Value::Bool(true));
        assert_eq!(eval_full("(and (> 5 3) (< 4 2))"), Value::Bool(false));
        assert_eq!(eval_full("(or (= 1 2) (> 5 3))"), Value::Bool(true));
        assert_eq!(eval_full("(not (= 5 3))"), Value::Bool(true));
        assert_eq!(eval_full("(not (= 5 5))"), Value::Bool(false));
    }
}

#[cfg(test)]
mod variable_tests {
    use super::*;

    #[test]
    fn test_integer_returning_variable_expressions() {
        // Test expressions with variables that return integers (for eval_expression)
        let lexer = Lexer::new("(define x 10)".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        let env = Environment::new_root();
        evaluator::risp_eval_program(&asts, &env);

        // Now test expressions using that variable
        let lexer = Lexer::new("(+ x 5)".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        let result = evaluator::risp_eval_program(&asts, &env);
        assert_eq!(result, Value::Int(15));
    }
}

#[cfg(test)]
mod conditional_tests {
    use super::*;

    #[test]
    fn test_if_statements() {
        // If returning integers
        assert_eq!(eval_full("(if #t 42 99)"), Value::Int(42));
        assert_eq!(eval_full("(if #f 42 99)"), Value::Int(99));

        // If with 2 arguments (returns #f when condition false)
        assert_eq!(eval_full("(if #t 100)"), Value::Int(100));
        assert_eq!(eval_full("(if #f 100)"), Value::Bool(false));

        // If with comparisons
        assert_eq!(eval_full("(if (> 5 3) 1 0)"), Value::Int(1));
        assert_eq!(eval_full("(if (< 5 3) 1 0)"), Value::Int(0));

        // If returning booleans
        assert_eq!(eval_full("(if #t #t #f)"), Value::Bool(true));
        assert_eq!(eval_full("(if #f #t #f)"), Value::Bool(false));
    }

    #[test]
    fn test_nested_conditionals() {
        assert_eq!(eval_full("(if (> 5 3) (if #t 42 0) 99)"), Value::Int(42));
        assert_eq!(
            eval_full("(if (< 5 3) 100 (if #t 200 300))"),
            Value::Int(200)
        );
    }

    #[test]
    fn test_arithmetic_with_conditionals() {
        assert_eq!(eval_full("(+ 10 (if #t 5 0))"), Value::Int(15));
        assert_eq!(eval_full("(+ 10 (if #f 5 0))"), Value::Int(10));
        assert_eq!(eval_full("(* (if (> 3 1) 2 1) 7)"), Value::Int(14));
    }
}

#[cfg(test)]
mod closure_semantics_tests {
    use super::*;

    #[test]
    fn test_simple_lambda_call() {
        // Step 7.1: Simple lambda call - ((lambda (x) (+ x 1)) 41) => 42
        assert_eq!(eval_full("((lambda (x) (+ x 1)) 41)"), Value::Int(42));

        // Additional simple cases
        assert_eq!(eval_full("((lambda (x) (* x 2)) 6)"), Value::Int(12));
        assert_eq!(eval_full("((lambda (a b) (+ a b)) 10 20)"), Value::Int(30));
        assert_eq!(eval_full("((lambda () 42))"), Value::Int(42));
    }

    #[test]
    fn test_closure_capture() {
        // Step 7.2: Closure capture
        // (define make-adder (lambda (k) (lambda (x) (+ x k))))
        // (define add5 (make-adder 5))
        // (add5 37) => 42

        let env = Environment::new_root();

        // Define make-adder
        let lexer = Lexer::new("(define make-adder (lambda (k) (lambda (x) (+ x k))))".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Create add5
        let lexer = Lexer::new("(define add5 (make-adder 5))".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test add5
        let lexer = Lexer::new("(add5 37)".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        let result = evaluator::risp_eval_program(&asts, &env);
        assert_eq!(result, Value::Int(42));

        // Test another closure from same make-adder
        let lexer = Lexer::new("(define add10 (make-adder 10))".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        let lexer = Lexer::new("(add10 32)".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        let result = evaluator::risp_eval_program(&asts, &env);
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_recursive_factorial() {
        // Step 7.3: Recursive factorial
        // (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
        // (fact 6) => 720

        let env = Environment::new_root();

        // Define factorial function
        let lexer = Lexer::new(
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))".to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test factorial cases
        let test_cases = vec![
            ("(fact 0)", 1),
            ("(fact 1)", 1),
            ("(fact 2)", 2),
            ("(fact 3)", 6),
            ("(fact 4)", 24),
            ("(fact 5)", 120),
            ("(fact 6)", 720),
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_nested_closures() {
        // Test more complex closure nesting
        let env = Environment::new_root();

        // (define make-multiplier (lambda (m) (lambda (x) (* x m))))
        let lexer =
            Lexer::new("(define make-multiplier (lambda (m) (lambda (x) (* x m))))".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // (define double (make-multiplier 2))
        let lexer = Lexer::new("(define double (make-multiplier 2))".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // (double 21) => 42
        let lexer = Lexer::new("(double 21)".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        let result = evaluator::risp_eval_program(&asts, &env);
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_function_definition_sugar_with_recursion() {
        // Test that function definition sugar works with recursion
        let env = Environment::new_root();

        // (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
        let lexer = Lexer::new(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))".to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test fibonacci sequence
        let fib_cases = vec![
            ("(fib 0)", 0),
            ("(fib 1)", 1),
            ("(fib 2)", 1),
            ("(fib 3)", 2),
            ("(fib 4)", 3),
            ("(fib 5)", 5),
            ("(fib 6)", 8),
        ];

        for (expr, expected) in fib_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }
}

#[cfg(test)]
mod tail_call_optimization_tests {
    use super::*;

    #[test]
    fn test_deep_factorial_recursion() {
        // Test that factorial works with deeper recursion without stack overflow
        let env = Environment::new_root();

        // Define factorial function
        let lexer = Lexer::new(
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))".to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test larger factorial values that would cause stack overflow without TCO
        let test_cases = vec![("(fact 10)", 3628800), ("(fact 12)", 479001600)];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_deep_fibonacci_recursion() {
        // Test that fibonacci works with deeper recursion
        let env = Environment::new_root();

        // Define fibonacci function
        let lexer = Lexer::new(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))".to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test fibonacci values that would be slow but shouldn't stack overflow
        let fib_cases = vec![("(fib 10)", 55), ("(fib 15)", 610)];

        for (expr, expected) in fib_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_tail_recursive_countdown() {
        // Test a simple tail-recursive function
        let env = Environment::new_root();

        // Define a tail-recursive countdown function
        let lexer =
            Lexer::new("(define (countdown n) (if (= n 0) 0 (countdown (- n 1))))".to_string());
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test with larger values that would cause stack overflow without TCO
        let test_cases = vec![
            ("(countdown 100)", 0),
            ("(countdown 500)", 0),
            ("(countdown 1000)", 0),
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_tail_recursive_sum() {
        // Test a tail-recursive sum function
        let env = Environment::new_root();

        // Define a tail-recursive sum from 1 to n
        let lexer = Lexer::new(
            "(define (sum-to n) (define (sum-helper n acc) (if (= n 0) acc (sum-helper (- n 1) (+ acc n)))) (sum-helper n 0))".to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test sum calculations
        let test_cases = vec![
            ("(sum-to 10)", 55),    // 1+2+...+10 = 55
            ("(sum-to 100)", 5050), // 1+2+...+100 = 5050
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_nested_lambda_deep_calls() {
        // Test that nested lambda calls work with TCO
        let env = Environment::new_root();

        // Define a function that creates deeply nested calls
        let lexer = Lexer::new(
            "(define (deep-call n) (if (= n 0) 42 ((lambda (x) (deep-call (- x 1))) n)))"
                .to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test with depth that would cause stack overflow without TCO
        let test_cases = vec![("(deep-call 50)", 42), ("(deep-call 100)", 42)];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_if_branch_tail_calls() {
        // Test that both if branches use tail calls correctly
        let env = Environment::new_root();

        // Define functions that use if branches for tail calls
        let lexer = Lexer::new(
            "(define (even-odd n) (if (= n 0) 1 (if (= n 1) 0 (even-odd (- n 2)))))".to_string(),
        );
        let mut parser = Parser::new(lexer);
        let asts = parser.parse();
        evaluator::risp_eval_program(&asts, &env);

        // Test with values that exercise both branches
        let test_cases = vec![
            ("(even-odd 100)", 1), // 100 is even -> 1
            ("(even-odd 101)", 0), // 101 is odd -> 0
            ("(even-odd 200)", 1), // 200 is even -> 1
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer);
            let asts = parser.parse();
            let result = evaluator::risp_eval_program(&asts, &env);
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }
}
