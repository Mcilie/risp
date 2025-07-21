use risp::eval_expression;
use risp::{env::Environment, evaluator::risp_eval, lexer::Lexer, parser::Parser, value::Value};

// Helper function to test expressions that might return booleans
fn eval_full(input: &str) -> Value {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    let mut env = Environment::new();
    risp_eval(&ast, &mut env)
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
        let ast = parser.parse();
        let mut env = Environment::new();
        risp_eval(&ast, &mut env);

        // Now test expressions using that variable
        let lexer = Lexer::new("(+ x 5)".to_string());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse();
        let result = risp_eval(&ast, &mut env);
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
