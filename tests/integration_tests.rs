use risp::eval_expression;

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
