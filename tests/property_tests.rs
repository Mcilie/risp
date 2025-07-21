use proptest::prelude::*;
use risp::eval_expression;
use risp::{env::Environment, evaluator::risp_eval, lexer::Lexer, parser::Parser, value::Value};

// Helper function to format numbers correctly for Lisp
fn format_number(n: i32) -> String {
    if n < 0 {
        format!("(- {})", -n)
    } else {
        n.to_string()
    }
}

// Helper function to evaluate expressions that return any Value type
fn eval_full(input: &str) -> Value {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    let mut env = Environment::new();
    risp_eval(&ast, &mut env)
}

// Helper function to format boolean values
fn format_bool(b: bool) -> String {
    if b {
        "#t".to_string()
    } else {
        "#f".to_string()
    }
}

proptest! {
    // Existing arithmetic property tests
    #[test]
    fn test_addition_commutativity(a in -100..100i32, b in -100..100i32) {
        let expr1 = format!("(+ {} {})", format_number(a), format_number(b));
        let expr2 = format!("(+ {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    #[test]
    fn test_multiplication_commutativity(a in -50..50i32, b in -50..50i32) {
        let expr1 = format!("(* {} {})", format_number(a), format_number(b));
        let expr2 = format!("(* {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    #[test]
    fn test_addition_associativity(a in -50..50i32, b in -50..50i32, c in -50..50i32) {
        let expr1 = format!("(+ (+ {} {}) {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ {} (+ {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    #[test]
    fn test_multiplication_associativity(a in -10..10i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(* (* {} {}) {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(* {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    #[test]
    fn test_additive_identity(a in -100..100i32) {
        let expr1 = format!("(+ {} 0)", format_number(a));
        let expr2 = format!("(+ 0 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1), a);
        prop_assert_eq!(eval_expression(&expr2), a);
    }

    #[test]
    fn test_multiplicative_identity(a in -100..100i32) {
        let expr1 = format!("(* {} 1)", format_number(a));
        let expr2 = format!("(* 1 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1), a);
        prop_assert_eq!(eval_expression(&expr2), a);
    }

    #[test]
    fn test_subtraction_identity(a in -100..100i32) {
        let expr = format!("(- {} 0)", format_number(a));
        prop_assert_eq!(eval_expression(&expr), a);
    }

    #[test]
    fn test_self_subtraction(a in -100..100i32) {
        let expr = format!("(- {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_expression(&expr), 0);
    }

    #[test]
    fn test_self_division(a in -100..100i32) {
        prop_assume!(a != 0);
        let expr = format!("(/ {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_expression(&expr), 1);
    }

    #[test]
    fn test_double_negation(a in -100..100i32) {
        let expr = format!("(- (- {}))", format_number(a));
        prop_assert_eq!(eval_expression(&expr), a);
    }

    #[test]
    fn test_distributivity(a in -10..10i32, b in -10..10i32, c in -10..10i32) {
        let expr1 = format!("(* {} (+ {} {}))", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ (* {} {}) (* {} {}))", format_number(a), format_number(b), format_number(a), format_number(c));
        prop_assert_eq!(eval_expression(&expr1), eval_expression(&expr2));
    }

    // NEW: Boolean operation properties
    #[test]
    fn test_and_commutativity(a: bool, b: bool) {
        let expr1 = format!("(and {} {})", format_bool(a), format_bool(b));
        let expr2 = format!("(and {} {})", format_bool(b), format_bool(a));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_or_commutativity(a: bool, b: bool) {
        let expr1 = format!("(or {} {})", format_bool(a), format_bool(b));
        let expr2 = format!("(or {} {})", format_bool(b), format_bool(a));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_and_associativity(a: bool, b: bool, c: bool) {
        let expr1 = format!("(and (and {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(and {} (and {} {}))", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_or_associativity(a: bool, b: bool, c: bool) {
        let expr1 = format!("(or (or {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(or {} (or {} {}))", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_double_negation_bool(a: bool) {
        let expr = format!("(not (not {}))", format_bool(a));
        prop_assert_eq!(eval_full(&expr), Value::Bool(a));
    }

    // De Morgan's Laws
    #[test]
    fn test_demorgans_law_and(a: bool, b: bool) {
        // not (a and b) = (not a) or (not b)
        let expr1 = format!("(not (and {} {}))", format_bool(a), format_bool(b));
        let expr2 = format!("(or (not {}) (not {}))", format_bool(a), format_bool(b));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_demorgans_law_or(a: bool, b: bool) {
        // not (a or b) = (not a) and (not b)
        let expr1 = format!("(not (or {} {}))", format_bool(a), format_bool(b));
        let expr2 = format!("(and (not {}) (not {}))", format_bool(a), format_bool(b));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    // Boolean identity laws
    #[test]
    fn test_and_identity(a: bool) {
        // a and true = a
        let expr = format!("(and {} #t)", format_bool(a));
        prop_assert_eq!(eval_full(&expr), Value::Bool(a));
    }

    #[test]
    fn test_or_identity(a: bool) {
        // a or false = a
        let expr = format!("(or {} #f)", format_bool(a));
        prop_assert_eq!(eval_full(&expr), Value::Bool(a));
    }

    // Comparison properties
    #[test]
    fn test_equality_reflexivity(a in -100..100i32) {
        let expr = format!("(= {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_full(&expr), Value::Bool(true));
    }

    #[test]
    fn test_equality_symmetry(a in -100..100i32, b in -100..100i32) {
        let expr1 = format!("(= {} {})", format_number(a), format_number(b));
        let expr2 = format!("(= {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_less_than_antisymmetry(a in -100..100i32, b in -100..100i32) {
        // If a < b, then not (b < a)
        let expr1 = format!("(< {} {})", format_number(a), format_number(b));
        let expr2 = format!("(< {} {})", format_number(b), format_number(a));

        let result1 = eval_full(&expr1);
        let result2 = eval_full(&expr2);

        if result1 == Value::Bool(true) {
            prop_assert_eq!(result2, Value::Bool(false));
        }
    }

    #[test]
    fn test_greater_than_antisymmetry(a in -100..100i32, b in -100..100i32) {
        // If a > b, then not (b > a)
        let expr1 = format!("(> {} {})", format_number(a), format_number(b));
        let expr2 = format!("(> {} {})", format_number(b), format_number(a));

        let result1 = eval_full(&expr1);
        let result2 = eval_full(&expr2);

        if result1 == Value::Bool(true) {
            prop_assert_eq!(result2, Value::Bool(false));
        }
    }

    // If statement properties
    #[test]
    fn test_if_true_branch(a in -100..100i32, b in -100..100i32) {
        let expr = format!("(if #t {} {})", format_number(a), format_number(b));
        prop_assert_eq!(eval_full(&expr), Value::Int(a));
    }

    #[test]
    fn test_if_false_branch(a in -100..100i32, b in -100..100i32) {
        let expr = format!("(if #f {} {})", format_number(a), format_number(b));
        prop_assert_eq!(eval_full(&expr), Value::Int(b));
    }

    #[test]
    fn test_if_boolean_condition(a: bool, b in -100..100i32, c in -100..100i32) {
        let expr = format!("(if {} {} {})", format_bool(a), format_number(b), format_number(c));
        let expected = if a { Value::Int(b) } else { Value::Int(c) };
        prop_assert_eq!(eval_full(&expr), expected);
    }

    // Truthiness properties (everything except #f is truthy)
    #[test]
    fn test_number_truthiness(n in -100..100i32) {
        // All numbers (including 0) are truthy
        let expr = format!("(if {} 1 0)", format_number(n));
        prop_assert_eq!(eval_full(&expr), Value::Int(1));
    }

    #[test]
    fn test_boolean_truthiness(b: bool) {
        let expr = format!("(if {} 1 0)", format_bool(b));
        let expected = if b { Value::Int(1) } else { Value::Int(0) };
        prop_assert_eq!(eval_full(&expr), expected);
    }

    // Mixed boolean-arithmetic properties
    #[test]
    fn test_comparison_with_arithmetic(a in -50..50i32, b in -50..50i32) {
        // (= (+ a b) (+ b a)) should always be true (commutativity)
        let expr = format!("(= (+ {} {}) (+ {} {}))", format_number(a), format_number(b), format_number(b), format_number(a));
        prop_assert_eq!(eval_full(&expr), Value::Bool(true));
    }

    #[test]
    fn test_conditional_arithmetic(a in -20..20i32, b in -20..20i32, c in -20..20i32) {
        // Test that arithmetic works correctly inside conditionals
        let condition = a > b;
        let expr = format!("(if {} {} {})", format_bool(condition), format_number(a + c), format_number(b + c));
        let expected = if condition { Value::Int(a + c) } else { Value::Int(b + c) };
        prop_assert_eq!(eval_full(&expr), expected);
    }

    // Variable arity boolean operations
    #[test]
    fn test_variable_arity_and(a: bool, b: bool, c: bool) {
        let expr1 = format!("(and {} {} {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(and (and {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    #[test]
    fn test_variable_arity_or(a: bool, b: bool, c: bool) {
        let expr1 = format!("(or {} {} {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(or (or {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1), eval_full(&expr2));
    }

    // Test that parsing never panics on well-formed expressions
    #[test]
    fn test_parsing_robustness_with_booleans(
        a in -50..50i32,
        b in 1..50i32,  // Avoid division by zero
        c in -50..50i32,
        bool_a: bool,
        bool_b: bool
    ) {
        let expressions = vec![
            format!("(and {} (> {} {}))", format_bool(bool_a), format_number(a), format_number(c)),
            format!("(or {} (< {} {}))", format_bool(bool_b), format_number(a), format_number(b)),
            format!("(if (= {} {}) {} {})", format_number(a), format_number(c), format_number(b), format_number(c)),
            format!("(not (and {} {}))", format_bool(bool_a), format_bool(bool_b)),
            format!("(+ {} (if {} {} {}))", format_number(a), format_bool(bool_a), format_number(b), format_number(c)),
            format!("(if (> (+ {} {}) {}) #t #f)", format_number(a), format_number(b), format_number(c)),
        ];

        for expr in expressions {
            // Should not panic
            let _result = eval_full(&expr);
        }
    }
}
