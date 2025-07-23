use risp::eval_expression;
use risp::{env::Environment, evaluator, lexer::Lexer, parser::Parser, value::Value};

// Helper function to test expressions that might return booleans
fn eval_full(input: &str) -> Result<Value, risp::error::RispError> {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer)?;
    let asts = parser.parse()?;
    let env = Environment::new_root();

    // Evaluate all expressions, returning the last one
    if asts.is_empty() {
        return Ok(Value::Bool(false));
    }

    let mut result = Value::Bool(false);
    for expr in &asts {
        result = evaluator::risp_eval(expr, &env)?;
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_numbers() {
        assert_eq!(eval_expression("42").unwrap(), 42);
        assert_eq!(eval_expression("0").unwrap(), 0);
        assert_eq!(eval_expression("123").unwrap(), 123);
    }

    #[test]
    fn test_basic_addition() {
        assert_eq!(eval_expression("(+ 2 3)").unwrap(), 5);
        assert_eq!(eval_expression("(+ 10 5)").unwrap(), 15);
        assert_eq!(eval_expression("(+ 1 2 3)").unwrap(), 6);
    }

    #[test]
    fn test_basic_subtraction() {
        assert_eq!(eval_expression("(- 5 3)").unwrap(), 2);
        assert_eq!(eval_expression("(- 10 5)").unwrap(), 5);
        assert_eq!(eval_expression("(- 10 3 2)").unwrap(), 5); // 10 - 3 - 2 = 5
    }

    #[test]
    fn test_basic_multiplication() {
        assert_eq!(eval_expression("(* 3 4)").unwrap(), 12);
        assert_eq!(eval_expression("(* 2 5)").unwrap(), 10);
        assert_eq!(eval_expression("(* 2 3 4)").unwrap(), 24);
    }

    #[test]
    fn test_basic_division() {
        assert_eq!(eval_expression("(/ 8 2)").unwrap(), 4);
        assert_eq!(eval_expression("(/ 15 3)").unwrap(), 5);
        assert_eq!(eval_expression("(/ 24 3)").unwrap(), 8); // Can't chain division easily in Lisp
    }

    #[test]
    fn test_unary_operators() {
        // Basic unary negation
        assert_eq!(eval_expression("(- 5)").unwrap(), -5);
        assert_eq!(eval_expression("(- 0)").unwrap(), 0);
        assert_eq!(eval_expression("(- 42)").unwrap(), -42);

        // Unary with nested expressions
        assert_eq!(eval_expression("(- (+ 2 3))").unwrap(), -5);
        assert_eq!(eval_expression("(- (- 10 4))").unwrap(), -6);
        assert_eq!(eval_expression("(- (* 3 4))").unwrap(), -12);

        // Double negation
        assert_eq!(eval_expression("(- (- 5))").unwrap(), 5);
        assert_eq!(eval_expression("(- (- (- 3)))").unwrap(), -3);

        // Unary in binary expressions
        assert_eq!(eval_expression("(+ 5 (- 3))").unwrap(), 2);
        assert_eq!(eval_expression("(+ (- 2) 7)").unwrap(), 5);
        assert_eq!(eval_expression("(- 10 (- 3))").unwrap(), 13);
        assert_eq!(eval_expression("(- (- 5) (- 2))").unwrap(), -3);

        // Unary with multiplication/division
        assert_eq!(eval_expression("(* (- 2) 3)").unwrap(), -6);
        assert_eq!(eval_expression("(* 2 (- 3))").unwrap(), -6);
        assert_eq!(eval_expression("(/ (- 8) 2)").unwrap(), -4);
        assert_eq!(eval_expression("(/ 8 (- 2))").unwrap(), -4);

        // Complex expressions with unary
        assert_eq!(eval_expression("(+ (- 2) (* 3 4))").unwrap(), 10);
        assert_eq!(eval_expression("(* (- (+ 2 3)) 4)").unwrap(), -20);
        assert_eq!(eval_expression("(+ (* (- 5) (- 3)) 1)").unwrap(), 16);
    }

    #[test]
    fn test_nested_expressions() {
        // Test nested S-expressions (no precedence - explicit grouping)
        assert_eq!(eval_expression("(+ 2 (* 3 4))").unwrap(), 14);
        assert_eq!(eval_expression("(- 10 (* 2 3))").unwrap(), 4);
        assert_eq!(eval_expression("(+ (* 2 3) 4)").unwrap(), 10);
        assert_eq!(eval_expression("(+ (/ 20 4) 1)").unwrap(), 6);
    }

    #[test]
    fn test_parentheses_grouping() {
        // All grouping is now explicit with S-expressions
        assert_eq!(eval_expression("(* (+ 2 3) 4)").unwrap(), 20);
        assert_eq!(eval_expression("(* 2 (+ 3 4))").unwrap(), 14);
        assert_eq!(eval_expression("(/ (- 10 2) (+ 3 1))").unwrap(), 2);
        assert_eq!(eval_expression("(* (+ 2 3) 4)").unwrap(), 20);
    }

    #[test]
    fn test_complex_expressions() {
        assert_eq!(eval_expression("(- (+ 2 (* 3 4)) 1)").unwrap(), 13);
        assert_eq!(eval_expression("(* (+ 2 3) (- 4 1))").unwrap(), 15);
        assert_eq!(eval_expression("(+ (- 10 (* 2 3)) 1)").unwrap(), 5);
        assert_eq!(eval_expression("(+ (/ 20 (+ 2 2)) (* 3 2))").unwrap(), 11);
    }

    #[test]
    fn test_whitespace_handling() {
        assert_eq!(eval_expression("(+ 2 3)").unwrap(), 5);
        assert_eq!(eval_expression("  ( +   2   3  )  ").unwrap(), 5);
        assert_eq!(eval_expression("(* 2 (+ 3 4))").unwrap(), 14);
        assert_eq!(eval_expression("\t(\n+\r2\t3)\n").unwrap(), 5);
    }

    #[test]
    fn test_variable_arity() {
        // Test Lisp's variable arity functions
        assert_eq!(eval_expression("(+ 1 2 3 4)").unwrap(), 10);
        assert_eq!(eval_expression("(+ 2 3 4)").unwrap(), 9);
        assert_eq!(eval_expression("(* 2 3 4)").unwrap(), 24);
        assert_eq!(eval_expression("(* 1 1 1 1)").unwrap(), 1);
    }

    #[test]
    fn test_deeply_nested() {
        assert_eq!(eval_expression("(* (+ 2 3) (+ 4 1))").unwrap(), 25);
        assert_eq!(eval_expression("(+ 1 2)").unwrap(), 3);
        assert_eq!(eval_expression("(* 2 (+ (+ 3 4) (- 5 2)))").unwrap(), 20);
    }

    #[test]
    fn test_edge_cases() {
        assert_eq!(eval_expression("(* 0 100)").unwrap(), 0);
        assert_eq!(eval_expression("(+ 100 0)").unwrap(), 100);
        assert_eq!(eval_expression("(* 1 1 1 1)").unwrap(), 1);
        assert_eq!(eval_expression("0").unwrap(), 0);
    }
}

#[cfg(test)]
mod boolean_tests {
    use super::*;

    #[test]
    fn test_boolean_literals() {
        assert_eq!(eval_full("#t").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("#f").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_logical_operations() {
        // AND operation
        assert_eq!(eval_full("(and #t #t)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(and #t #f)").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(and #f #t)").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(and #f #f)").unwrap(), Value::Bool(false));

        // OR operation
        assert_eq!(eval_full("(or #t #t)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(or #t #f)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(or #f #t)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(or #f #f)").unwrap(), Value::Bool(false));

        // NOT operation
        assert_eq!(eval_full("(not #t)").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(not #f)").unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_comparison_operations() {
        // Equality
        assert_eq!(eval_full("(= 5 5)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(= 3 7)").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(= 1 1 1)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(= 1 2 1)").unwrap(), Value::Bool(false));

        // Less than
        assert_eq!(eval_full("(< 3 5)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(< 5 3)").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(< 5 5)").unwrap(), Value::Bool(false));

        // Greater than
        assert_eq!(eval_full("(> 5 3)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(> 3 5)").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(> 5 5)").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_complex_boolean_expressions() {
        assert_eq!(eval_full("(and (> 5 3) (< 2 4))").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(and (> 5 3) (< 4 2))").unwrap(), Value::Bool(false));
        assert_eq!(eval_full("(or (= 1 2) (> 5 3))").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(not (= 5 3))").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(not (= 5 5))").unwrap(), Value::Bool(false));
    }
}

#[cfg(test)]
mod variable_tests {
    use super::*;

    #[test]
    fn test_integer_returning_variable_expressions() {
        // Test expressions with variables that return integers (for eval_expression)
        let lexer = Lexer::new("(define x 10)".to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let env = Environment::new_root();
        
        // Evaluate define statement
        for expr in &asts {
            evaluator::risp_eval(expr, &env).unwrap();
        }

        // Now test expressions using that variable
        let lexer = Lexer::new("(+ x 5)".to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = evaluator::risp_eval(&asts[0], &env).unwrap();
        assert_eq!(result, Value::Int(15));
    }
}

#[cfg(test)]
mod conditional_tests {
    use super::*;

    #[test]
    fn test_if_statements() {
        // If returning integers
        assert_eq!(eval_full("(if #t 42 99)").unwrap(), Value::Int(42));
        assert_eq!(eval_full("(if #f 42 99)").unwrap(), Value::Int(99));

        // If with 2 arguments (returns #f when condition false)
        assert_eq!(eval_full("(if #t 100)").unwrap(), Value::Int(100));
        assert_eq!(eval_full("(if #f 100)").unwrap(), Value::Bool(false));

        // If with comparisons
        assert_eq!(eval_full("(if (> 5 3) 1 0)").unwrap(), Value::Int(1));
        assert_eq!(eval_full("(if (< 5 3) 1 0)").unwrap(), Value::Int(0));

        // If returning booleans
        assert_eq!(eval_full("(if #t #t #f)").unwrap(), Value::Bool(true));
        assert_eq!(eval_full("(if #f #t #f)").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_nested_conditionals() {
        assert_eq!(eval_full("(if (> 5 3) (if #t 42 0) 99)").unwrap(), Value::Int(42));
        assert_eq!(
            eval_full("(if (< 5 3) 100 (if #t 200 300))").unwrap(),
            Value::Int(200)
        );
    }

    #[test]
    fn test_arithmetic_with_conditionals() {
        assert_eq!(eval_full("(+ 10 (if #t 5 0))").unwrap(), Value::Int(15));
        assert_eq!(eval_full("(+ 10 (if #f 5 0))").unwrap(), Value::Int(10));
        assert_eq!(eval_full("(* (if (> 3 1) 2 1) 7)").unwrap(), Value::Int(14));
    }
}

// Helper function for closure tests
fn eval_program_in_env(statements: &[&str], env: &std::rc::Rc<Environment>) -> Result<Value, risp::error::RispError> {
    let mut result = Value::Bool(false);
    
    for statement in statements {
        let lexer = Lexer::new(statement.to_string());
        let mut parser = Parser::new(lexer)?;
        let asts = parser.parse()?;
        
        for expr in &asts {
            result = evaluator::risp_eval(expr, env)?;
        }
    }
    
    Ok(result)
}

#[cfg(test)]
mod closure_semantics_tests {
    use super::*;

    #[test]
    fn test_simple_lambda_call() {
        // Step 7.1: Simple lambda call - ((lambda (x) (+ x 1)) 41) => 42
        assert_eq!(eval_full("((lambda (x) (+ x 1)) 41)").unwrap(), Value::Int(42));

        // Additional simple cases
        assert_eq!(eval_full("((lambda (x) (* x 2)) 6)").unwrap(), Value::Int(12));
        assert_eq!(eval_full("((lambda (a b) (+ a b)) 10 20)").unwrap(), Value::Int(30));
        assert_eq!(eval_full("((lambda () 42))").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_closure_capture() {
        // Step 7.2: Closure capture
        let env = Environment::new_root();

        let statements = vec![
            "(define make-adder (lambda (k) (lambda (x) (+ x k))))",
            "(define add5 (make-adder 5))",
        ];
        
        eval_program_in_env(&statements, &env).unwrap();

        // Test add5
        let lexer = Lexer::new("(add5 37)".to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = evaluator::risp_eval(&asts[0], &env).unwrap();
        assert_eq!(result, Value::Int(42));

        // Test another closure from same make-adder
        let statements2 = vec!["(define add10 (make-adder 10))"];
        eval_program_in_env(&statements2, &env).unwrap();

        let lexer = Lexer::new("(add10 32)".to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = evaluator::risp_eval(&asts[0], &env).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_recursive_factorial() {
        // Step 7.3: Recursive factorial
        let env = Environment::new_root();

        // Define factorial function
        let statements = vec![
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

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
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_nested_closures() {
        // Test more complex closure nesting
        let env = Environment::new_root();

        let statements = vec![
            "(define make-multiplier (lambda (m) (lambda (x) (* x m))))",
            "(define double (make-multiplier 2))",
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // (double 21) => 42
        let lexer = Lexer::new("(double 21)".to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = evaluator::risp_eval(&asts[0], &env).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_function_definition_sugar_with_recursion() {
        // Test that function definition sugar works with recursion
        let env = Environment::new_root();

        let statements = vec![
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

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
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
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

        let statements = vec![
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test larger factorial values that would cause stack overflow without TCO
        let test_cases = vec![("(fact 10)", 3628800), ("(fact 12)", 479001600)];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_deep_fibonacci_recursion() {
        // Test that fibonacci works with deeper recursion
        let env = Environment::new_root();

        let statements = vec![
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test fibonacci values that would be slow but shouldn't stack overflow
        let fib_cases = vec![("(fib 10)", 55), ("(fib 15)", 610)];

        for (expr, expected) in fib_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_tail_recursive_countdown() {
        // Test a simple tail-recursive function
        let env = Environment::new_root();

        let statements = vec![
            "(define (countdown n) (if (= n 0) 0 (countdown (- n 1))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test with larger values that would cause stack overflow without TCO
        let test_cases = vec![
            ("(countdown 100)", 0),
            ("(countdown 500)", 0),
            ("(countdown 1000)", 0),
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_tail_recursive_sum() {
        // Test a tail-recursive sum function
        let env = Environment::new_root();

        let statements = vec![
            "(define (sum-to n) (define (sum-helper n acc) (if (= n 0) acc (sum-helper (- n 1) (+ acc n)))) (sum-helper n 0))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test sum calculations
        let test_cases = vec![
            ("(sum-to 10)", 55),    // 1+2+...+10 = 55
            ("(sum-to 100)", 5050), // 1+2+...+100 = 5050
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_nested_lambda_deep_calls() {
        // Test that nested lambda calls work with TCO
        let env = Environment::new_root();

        let statements = vec![
            "(define (deep-call n) (if (= n 0) 42 ((lambda (x) (deep-call (- x 1))) n)))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test with depth that would cause stack overflow without TCO
        let test_cases = vec![("(deep-call 50)", 42), ("(deep-call 100)", 42)];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }

    #[test]
    fn test_if_branch_tail_calls() {
        // Test that both if branches use tail calls correctly
        let env = Environment::new_root();

        let statements = vec![
            "(define (even-odd n) (if (= n 0) 1 (if (= n 1) 0 (even-odd (- n 2)))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test with values that exercise both branches
        let test_cases = vec![
            ("(even-odd 100)", 1), // 100 is even -> 1
            ("(even-odd 101)", 0), // 101 is odd -> 0
            ("(even-odd 200)", 1), // 200 is even -> 1
        ];

        for (expr, expected) in test_cases {
            let lexer = Lexer::new(expr.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let asts = parser.parse().unwrap();
            let result = evaluator::risp_eval(&asts[0], &env).unwrap();
            assert_eq!(result, Value::Int(expected), "Failed for {}", expr);
        }
    }
}
