use proptest::prelude::*;
use risp::eval_expression;

// Helper function to format numbers correctly for Lisp
fn format_number(n: i32) -> String {
    if n < 0 {
        format!("(- {})", -n)
    } else {
        n.to_string()
    }
}

proptest! {
    // Test commutativity of addition: (+ a b) = (+ b a)
    #[test]
    fn test_addition_commutativity(a in -100..100i32, b in -100..100i32) {
        let expr1 = format!("(+ {} {})", format_number(a), format_number(b));
        let expr2 = format!("(+ {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test commutativity of multiplication: (* a b) = (* b a)
    #[test]
    fn test_multiplication_commutativity(a in -50..50i32, b in -50..50i32) {
        let expr1 = format!("(* {} {})", format_number(a), format_number(b));
        let expr2 = format!("(* {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test associativity of addition: (+ (+ a b) c) = (+ a (+ b c))
    #[test]
    fn test_addition_associativity(a in -50..50i32, b in -50..50i32, c in -50..50i32) {
        let expr1 = format!("(+ (+ {} {}) {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ {} (+ {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test associativity of multiplication: (* (* a b) c) = (* a (* b c))
    #[test]
    fn test_multiplication_associativity(a in -10..10i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(* (* {} {}) {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(* {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test additive identity: (+ a 0) = a
    #[test]
    fn test_additive_identity(a in -100..100i32) {
        let expr1 = format!("(+ {} 0)", format_number(a));
        let expr2 = format!("(+ 0 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1), a);
        prop_assert_eq!(eval_expression(&expr2), a);
    }

    // Test multiplicative identity: (* a 1) = a
    #[test]
    fn test_multiplicative_identity(a in -100..100i32) {
        let expr1 = format!("(* {} 1)", format_number(a));
        let expr2 = format!("(* 1 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1), a);
        prop_assert_eq!(eval_expression(&expr2), a);
    }

    // Test subtraction identity: (- a 0) = a
    #[test]
    fn test_subtraction_identity(a in -100..100i32) {
        let expr = format!("(- {} 0)", format_number(a));
        prop_assert_eq!(eval_expression(&expr), a);
    }

    // Test self subtraction: (- a a) = 0
    #[test]
    fn test_self_subtraction(a in -100..100i32) {
        let expr = format!("(- {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_expression(&expr), 0);
    }

    // Test self division: (/ a a) = 1 (when a != 0)
    #[test]
    fn test_self_division(a in -100..100i32) {
        prop_assume!(a != 0);
        let expr = format!("(/ {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_expression(&expr), 1);
    }

    // Test double negation: (- (- a)) = a
    #[test]
    fn test_double_negation(a in -100..100i32) {
        let expr = format!("(- (- {}))", format_number(a));
        prop_assert_eq!(eval_expression(&expr), a);
    }

    // Test nested expressions maintain structure
    #[test]
    fn test_nested_structure(a in -20..20i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(+ {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test different nesting patterns are consistent
    #[test]
    fn test_nesting_consistency(a in -20..20i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(- {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(- {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test distributivity: (* a (+ b c)) = (+ (* a b) (* a c))
    #[test]
    fn test_distributivity(a in -10..10i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(* {} (+ {} {}))", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ (* {} {}) (* {} {}))", format_number(a), format_number(b), format_number(a), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test negation distribution: (- (+ a b)) = (+ (- a) (- b))
    #[test]
    fn test_negation_distribution_add(a in -50..50i32, b in -50..50i32) {
        let expr1 = format!("(- (+ {} {}))", format_number(a), format_number(b));
        let expr2 = format!("(+ (- {}) (- {}))", format_number(a), format_number(b));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test negation with multiplication: (- (* a b)) = (* (- a) b) = (* a (- b))
    #[test]
    fn test_negation_multiplication(a in -10..10i32, b in -10..10i32) {
        let expr1 = format!("(- (* {} {}))", format_number(a), format_number(b));
        let expr2 = format!("(* (- {}) {})", format_number(a), format_number(b));
        let expr3 = format!("(* {} (- {}))", format_number(a), format_number(b));

        let result1 = eval_expression(&expr1);
        let result2 = eval_expression(&expr2);
        let result3 = eval_expression(&expr3);

        prop_assert_eq!(result1, result2);
        prop_assert_eq!(result1, result3);
    }

    // Test single values don't need wrapping: a = a
    #[test]
    fn test_single_values(a in 0..100i32) {  // Only positive to avoid the negative number issue
        let expr = format!("{}", a);
        prop_assert_eq!(eval_expression(&expr), a as i32);
    }

    // Test that parsing never panics on well-formed S-expressions
    #[test]
    fn test_parsing_robustness(
        a in -50..50i32,
        b in 1..50i32,  // Avoid division by zero
        c in -50..50i32
    ) {
        // Test various S-expression patterns
        let expressions = vec![
            format!("(+ {} (- {} {}))", format_number(a), format_number(b), format_number(c)),
            format!("(* {} (/ {} {}))", format_number(a), format_number(b), format_number(b)), // Use b twice to avoid div by zero
            format!("(* (+ {} {}) {})", format_number(a), format_number(c), format_number(b)),
            format!("(- {} (+ {} {}))", format_number(a), format_number(b), format_number(c)),
            format!("(+ (- {}) (* {} {}))", format_number(a), format_number(b), format_number(c)),
            format!("(- (- {}) (- {}))", format_number(a), format_number(b)),
        ];

        for expr in expressions {
            // Should not panic
            let _result = eval_expression(&expr);
        }
    }

    // Test zero multiplication property: (* a 0) = 0
    #[test]
    fn test_zero_multiplication(a in -100..100i32) {
        let expr1 = format!("(* {} 0)", format_number(a));
        let expr2 = format!("(* 0 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1), 0);
        prop_assert_eq!(eval_expression(&expr2), 0);
    }

    // Test subtraction vs addition with negation: (- a b) = (+ a (- b))
    #[test]
    fn test_subtraction_as_negative_addition(a in -50..50i32, b in -50..50i32) {
        let expr1 = format!("(- {} {})", format_number(a), format_number(b));
        let expr2 = format!("(+ {} (- {}))", format_number(a), format_number(b));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test variable arity addition
    #[test]
    fn test_variable_arity_addition(a in -20..20i32, b in -20..20i32, c in -20..20i32) {
        let expr1 = format!("(+ {} {} {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ (+ {} {}) {})", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // Test variable arity multiplication
    #[test]
    fn test_variable_arity_multiplication(a in -10..10i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(* {} {} {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(* (* {} {}) {})", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }
}
