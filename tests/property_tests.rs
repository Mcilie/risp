use proptest::prelude::*;
use risp::eval_expression;
use risp::{env::Environment, evaluator, lexer::Lexer, parser::Parser, value::Value};

// Helper function to format numbers correctly for Lisp
fn format_number(n: i64) -> String {
    if n < 0 {
        format!("(- {})", -n)
    } else {
        n.to_string()
    }
}

// Helper function to evaluate expressions that return any Value type
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

// Helper function to format boolean values
fn format_bool(b: bool) -> String {
    if b {
        "#t".to_string()
    } else {
        "#f".to_string()
    }
}

// Helper function for closure tests
fn eval_program_in_env(
    statements: &[&str],
    env: &std::rc::Rc<Environment>,
) -> Result<Value, risp::error::RispError> {
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

proptest! {
    // Existing arithmetic property tests
    #[test]
    fn test_addition_commutativity(a in -100..100i64, b in -100..100i64) {
        let expr1 = format!("(+ {} {})", format_number(a), format_number(b));
        let expr2 = format!("(+ {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), eval_expression(&expr2).unwrap());
    }

    #[test]
    fn test_multiplication_commutativity(a in -50..50i64, b in -50..50i64) {
        let expr1 = format!("(* {} {})", format_number(a), format_number(b));
        let expr2 = format!("(* {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), eval_expression(&expr2).unwrap());
    }

    #[test]
    fn test_addition_associativity(a in -50..50i64, b in -50..50i64, c in -50..50i64) {
        let expr1 = format!("(+ (+ {} {}) {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ {} (+ {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), eval_expression(&expr2).unwrap());
    }

    #[test]
    fn test_multiplication_associativity(a in -10..10i64, b in -10..10i64, c in -10..10i64) {
        let expr1 = format!("(* (* {} {}) {})", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(* {} (* {} {}))", format_number(a), format_number(b), format_number(c));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), eval_expression(&expr2).unwrap());
    }

    #[test]
    fn test_additive_identity(a in -100..100i64) {
        let expr1 = format!("(+ {} 0)", format_number(a));
        let expr2 = format!("(+ 0 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), a);
        prop_assert_eq!(eval_expression(&expr2).unwrap(), a);
    }

    #[test]
    fn test_multiplicative_identity(a in -100..100i64) {
        let expr1 = format!("(* {} 1)", format_number(a));
        let expr2 = format!("(* 1 {})", format_number(a));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), a);
        prop_assert_eq!(eval_expression(&expr2).unwrap(), a);
    }

    #[test]
    fn test_subtraction_identity(a in -100..100i64) {
        let expr = format!("(- {} 0)", format_number(a));
        prop_assert_eq!(eval_expression(&expr).unwrap(), a);
    }

    #[test]
    fn test_self_subtraction(a in -100..100i64) {
        let expr = format!("(- {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_expression(&expr).unwrap(), 0);
    }

    #[test]
    fn test_self_division(a in -100..100i64) {
        prop_assume!(a != 0);
        let expr = format!("(/ {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_expression(&expr).unwrap(), 1);
    }

    #[test]
    fn test_double_negation(a in -100..100i64) {
        let expr = format!("(- (- {}))", format_number(a));
        prop_assert_eq!(eval_expression(&expr).unwrap(), a);
    }

    #[test]
    fn test_distributivity(a in -10..10i64, b in -10..10i64, c in -10..10i64) {
        let expr1 = format!("(* {} (+ {} {}))", format_number(a), format_number(b), format_number(c));
        let expr2 = format!("(+ (* {} {}) (* {} {}))", format_number(a), format_number(b), format_number(a), format_number(c));
        prop_assert_eq!(eval_expression(&expr1).unwrap(), eval_expression(&expr2).unwrap());
    }

    // NEW: Boolean operation properties
    #[test]
    fn test_and_commutativity(a: bool, b: bool) {
        let expr1 = format!("(and {} {})", format_bool(a), format_bool(b));
        let expr2 = format!("(and {} {})", format_bool(b), format_bool(a));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_or_commutativity(a: bool, b: bool) {
        let expr1 = format!("(or {} {})", format_bool(a), format_bool(b));
        let expr2 = format!("(or {} {})", format_bool(b), format_bool(a));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_and_associativity(a: bool, b: bool, c: bool) {
        let expr1 = format!("(and (and {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(and {} (and {} {}))", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_or_associativity(a: bool, b: bool, c: bool) {
        let expr1 = format!("(or (or {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(or {} (or {} {}))", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_double_negation_bool(a: bool) {
        let expr = format!("(not (not {}))", format_bool(a));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Bool(a));
    }

    // De Morgan's Laws
    #[test]
    fn test_demorgans_law_and(a: bool, b: bool) {
        // not (a and b) = (not a) or (not b)
        let expr1 = format!("(not (and {} {}))", format_bool(a), format_bool(b));
        let expr2 = format!("(or (not {}) (not {}))", format_bool(a), format_bool(b));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_demorgans_law_or(a: bool, b: bool) {
        // not (a or b) = (not a) and (not b)
        let expr1 = format!("(not (or {} {}))", format_bool(a), format_bool(b));
        let expr2 = format!("(and (not {}) (not {}))", format_bool(a), format_bool(b));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    // Boolean identity laws
    #[test]
    fn test_and_identity(a: bool) {
        // a and true = a
        let expr = format!("(and {} #t)", format_bool(a));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Bool(a));
    }

    #[test]
    fn test_or_identity(a: bool) {
        // a or false = a
        let expr = format!("(or {} #f)", format_bool(a));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Bool(a));
    }

    // Comparison properties
    #[test]
    fn test_equality_reflexivity(a in -100..100i64) {
        let expr = format!("(= {} {})", format_number(a), format_number(a));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_equality_symmetry(a in -100..100i64, b in -100..100i64) {
        let expr1 = format!("(= {} {})", format_number(a), format_number(b));
        let expr2 = format!("(= {} {})", format_number(b), format_number(a));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_less_than_antisymmetry(a in -100..100i64, b in -100..100i64) {
        // If a < b, then not (b < a)
        let expr1 = format!("(< {} {})", format_number(a), format_number(b));
        let expr2 = format!("(< {} {})", format_number(b), format_number(a));

        let result1 = eval_full(&expr1).unwrap();
        let result2 = eval_full(&expr2).unwrap();

        if result1 == Value::Bool(true) {
            prop_assert_eq!(result2, Value::Bool(false));
        }
    }

    #[test]
    fn test_greater_than_antisymmetry(a in -100..100i64, b in -100..100i64) {
        // If a > b, then not (b > a)
        let expr1 = format!("(> {} {})", format_number(a), format_number(b));
        let expr2 = format!("(> {} {})", format_number(b), format_number(a));

        let result1 = eval_full(&expr1).unwrap();
        let result2 = eval_full(&expr2).unwrap();

        if result1 == Value::Bool(true) {
            prop_assert_eq!(result2, Value::Bool(false));
        }
    }

    // If statement properties
    #[test]
    fn test_if_true_branch(a in -100..100i64, b in -100..100i64) {
        let expr = format!("(if #t {} {})", format_number(a), format_number(b));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Int(a));
    }

    #[test]
    fn test_if_false_branch(a in -100..100i64, b in -100..100i64) {
        let expr = format!("(if #f {} {})", format_number(a), format_number(b));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Int(b));
    }

    #[test]
    fn test_if_boolean_condition(a: bool, b in -100..100i64, c in -100..100i64) {
        let expr = format!("(if {} {} {})", format_bool(a), format_number(b), format_number(c));
        let expected = if a { Value::Int(b) } else { Value::Int(c) };
        prop_assert_eq!(eval_full(&expr).unwrap(), expected);
    }

    // Truthiness properties (everything except #f is truthy)
    #[test]
    fn test_number_truthiness(n in -100..100i64) {
        // All numbers (including 0) are truthy
        let expr = format!("(if {} 1 0)", format_number(n));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Int(1));
    }

    #[test]
    fn test_boolean_truthiness(b: bool) {
        let expr = format!("(if {} 1 0)", format_bool(b));
        let expected = if b { Value::Int(1) } else { Value::Int(0) };
        prop_assert_eq!(eval_full(&expr).unwrap(), expected);
    }

    // Mixed boolean-arithmetic properties
    #[test]
    fn test_comparison_with_arithmetic(a in -50..50i64, b in -50..50i64) {
        // (= (+ a b) (+ b a)) should always be true (commutativity)
        let expr = format!("(= (+ {} {}) (+ {} {}))", format_number(a), format_number(b), format_number(b), format_number(a));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_conditional_arithmetic(a in -20..20i64, b in -20..20i64, c in -20..20i64) {
        // Test that arithmetic works correctly inside conditionals
        let condition = a > b;
        let expr = format!("(if {} {} {})", format_bool(condition), format_number(a + c), format_number(b + c));
        let expected = if condition { Value::Int(a + c) } else { Value::Int(b + c) };
        prop_assert_eq!(eval_full(&expr).unwrap(), expected);
    }

    // Variable arity boolean operations
    #[test]
    fn test_variable_arity_and(a: bool, b: bool, c: bool) {
        let expr1 = format!("(and {} {} {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(and (and {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    #[test]
    fn test_variable_arity_or(a: bool, b: bool, c: bool) {
        let expr1 = format!("(or {} {} {})", format_bool(a), format_bool(b), format_bool(c));
        let expr2 = format!("(or (or {} {}) {})", format_bool(a), format_bool(b), format_bool(c));
        prop_assert_eq!(eval_full(&expr1).unwrap(), eval_full(&expr2).unwrap());
    }

    // Test that parsing never panics on well-formed expressions
    #[test]
    fn test_parsing_robustness_with_booleans(
        a in -50..50i64,
        b in 1..50i64,  // Avoid division by zero
        c in -50..50i64,
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
            let _result = eval_full(&expr).unwrap();
        }
    }

    // NEW: Closure and Lambda Property Tests
    #[test]
    fn test_identity_function_property(x in -100..100i64) {
        // The identity function (lambda (x) x) should return its input unchanged
        let expr = format!("((lambda (x) x) {})", format_number(x));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Int(x));
    }

    #[test]
    fn test_constant_function_property(k in -50..50i64, x in -100..100i64) {
        // A constant function (lambda (x) k) should always return k regardless of x
        let expr = format!("((lambda (x) {}) {})", format_number(k), format_number(x));
        prop_assert_eq!(eval_full(&expr).unwrap(), Value::Int(k));
    }

    #[test]
    fn test_closure_capture_consistency(k in -20..20i64, x in -50..50i64) {
        // Closures should capture and use variables consistently
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define make-adder (lambda (k) (lambda (x) (+ x k))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Create specific adder
        let make_specific_adder = format!("(define adder (make-adder {}))", format_number(k));
        let statements2 = vec![make_specific_adder.as_str()];
        eval_program_in_env(&statements2, &env).unwrap();

        // Test the adder
        let test_expr = format!("(adder {})", format_number(x));
        let lexer = risp::lexer::Lexer::new(test_expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        prop_assert_eq!(result, Value::Int(x + k));
    }

    #[test]
    fn test_function_composition_property(a in -10..10i64, b in -10..10i64, x in -20..20i64) {
        // Test that function composition works correctly
        let env = risp::env::Environment::new_root();

        // Define f and g
        let f_def = format!("(define f (lambda (x) (+ x {})))", format_number(a));
        let g_def = format!("(define g (lambda (x) (* x {})))", format_number(b));
        let statements = vec![f_def.as_str(), g_def.as_str()];
        eval_program_in_env(&statements, &env).unwrap();

        // Test composition: (f (g x))
        let composition_expr = format!("(f (g {}))", format_number(x));
        let lexer = risp::lexer::Lexer::new(composition_expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        // Expected: (+ (* x b) a)
        let expected = (x * b) + a;
        prop_assert_eq!(result, Value::Int(expected));
    }

    #[test]
    fn test_factorial_mathematical_property(n in 0u32..8u32) {
        // Test that our recursive factorial matches mathematical factorial
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test factorial
        let expr = format!("(fact {})", n);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        // Calculate expected factorial
        let mut expected = 1i64;
        for i in 1..=n {
            expected *= i as i64;
        }

        prop_assert_eq!(result, Value::Int(expected));
    }

    #[test]
    fn test_higher_order_function_consistency(multiplier in 1..10i64, x in -20..20i64) {
        // Test that higher-order functions behave consistently
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define make-multiplier (lambda (m) (lambda (x) (* x m))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Create two identical multipliers
        let mult1_def = format!("(define mult1 (make-multiplier {}))", multiplier);
        let mult2_def = format!("(define mult2 (make-multiplier {}))", multiplier);
        let statements2 = vec![mult1_def.as_str(), mult2_def.as_str()];
        eval_program_in_env(&statements2, &env).unwrap();

        // Both should give same result
        let result1_expr = format!("(mult1 {})", format_number(x));
        let lexer = risp::lexer::Lexer::new(result1_expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result1 = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        let result2_expr = format!("(mult2 {})", format_number(x));
        let lexer = risp::lexer::Lexer::new(result2_expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result2 = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        let expected = Value::Int(x * multiplier);
        prop_assert_eq!(result1, expected.clone());
        prop_assert_eq!(result2, expected);
    }

    #[test]
    fn test_lambda_parameter_independence(a in -30..30i64, b in -30..30i64) {
        // Test that lambda parameters don't interfere with each other
        let expr = format!("((lambda (x y) (+ x y)) {} {})", format_number(a), format_number(b));
        let result = eval_full(&expr).unwrap();
        prop_assert_eq!(result, Value::Int(a + b));

        // Also test with reversed arguments
        let expr2 = format!("((lambda (x y) (+ x y)) {} {})", format_number(b), format_number(a));
        let result2 = eval_full(&expr2).unwrap();
        prop_assert_eq!(result2, Value::Int(a + b));
    }

    #[test]
    fn test_nested_lambda_scoping(outer in -15..15i64, inner in -15..15i64, x in -10..10i64) {
        // Test that nested lambdas maintain proper scoping
        let expr = format!("(((lambda (a) (lambda (b) (+ a b))) {}) {})",
                          format_number(outer), format_number(inner));
        let result = eval_full(&expr).unwrap();
        prop_assert_eq!(result, Value::Int(outer + inner));
    }

    #[test]
    fn test_recursive_function_base_case_property(base_value in -20..20i64) {
        // Test that recursive functions handle base cases correctly
        let env = risp::env::Environment::new_root();

        // Define a function that returns immediately for any input
        let func_def = format!("(define immediate (lambda (n) {}))", format_number(base_value));
        let statements = vec![func_def.as_str()];
        eval_program_in_env(&statements, &env).unwrap();

        // Should always return base_value regardless of input
        let test_input = 42;
        let expr = format!("(immediate {})", test_input);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        prop_assert_eq!(result, Value::Int(base_value));
    }

    // NEW: Tail-Call Optimization Property Tests
    #[test]
    fn test_factorial_deep_recursion_property(n in 0u32..15u32) {
        // Test that factorial works correctly with deeper recursion (TCO should prevent stack overflow)
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test factorial
        let expr = format!("(fact {})", n);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        // Calculate expected factorial
        let mut expected = 1i64;
        for i in 1..=n {
            expected *= i as i64;
        }

        // Only test if the result fits in i64 to avoid overflow
        if expected <= i64::MAX as i64 {
            prop_assert_eq!(result, Value::Int(expected as i64));
        }
    }

    #[test]
    fn test_tail_recursive_countdown_property(n in 0u32..2000u32) {
        // Test that tail-recursive countdown works for large values (should not stack overflow)
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define (countdown n) (if (= n 0) 0 (countdown (- n 1))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test countdown - should always return 0
        let expr = format!("(countdown {})", n);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        prop_assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn test_tail_recursive_sum_property(n in 1u32..200u32) {
        // Test that tail-recursive sum works correctly and efficiently
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define (sum-helper n acc) (if (= n 0) acc (sum-helper (- n 1) (+ acc n))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test sum from 1 to n
        let expr = format!("(sum-helper {} 0)", n);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        // Expected: n * (n + 1) / 2 (arithmetic series formula)
        let expected = (n * (n + 1) / 2) as i64;
        prop_assert_eq!(result, Value::Int(expected));
    }

    #[test]
    fn test_deep_if_branch_recursion_property(n in 1u32..500u32) {
        // Test that if branch tail calls work correctly for large recursion depths
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define (deep-if n) (if (= n 1) 42 (if (> n 1) (deep-if (- n 1)) 0)))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test - should always return 42 for n >= 1
        let expr = format!("(deep-if {})", n);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        prop_assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_lambda_tail_call_consistency_property(depth in 1u32..300u32) {
        // Test that lambda tail calls work consistently at different depths
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define (lambda-chain n) (if (= n 0) 100 ((lambda (x) (lambda-chain (- x 1))) n)))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test - should always return 100
        let expr = format!("(lambda-chain {})", depth);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        prop_assert_eq!(result, Value::Int(100));
    }

    #[test]
    fn test_recursive_even_odd_property(n in 0u32..1000u32) {
        // Test that a recursive even/odd function works correctly with TCO
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define (is-even n) (if (= n 0) 1 (if (= n 1) 0 (is-even (- n 2)))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test even/odd checker
        let expr = format!("(is-even {})", n);
        let lexer = risp::lexer::Lexer::new(expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        // Expected: 1 if even, 0 if odd
        let expected = if n % 2 == 0 { 1 } else { 0 };
        prop_assert_eq!(result, Value::Int(expected));
    }

    #[test]
    fn test_tco_preserves_mathematical_correctness(n in 5u32..20u32) {
        // Test that TCO doesn't change the mathematical correctness of recursive functions
        let env = risp::env::Environment::new_root();

        let statements = vec![
            "(define (recursive-sum n) (if (= n 0) 0 (+ n (recursive-sum (- n 1)))))",
            "(define (tail-sum n acc) (if (= n 0) acc (tail-sum (- n 1) (+ acc n))))"
        ];
        eval_program_in_env(&statements, &env).unwrap();

        // Test both functions give same result
        let recursive_expr = format!("(recursive-sum {})", n);
        let lexer = risp::lexer::Lexer::new(recursive_expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let recursive_result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        let tail_expr = format!("(tail-sum {} 0)", n);
        let lexer = risp::lexer::Lexer::new(tail_expr);
        let mut parser = risp::parser::Parser::new(lexer).unwrap();
        let asts = parser.parse().unwrap();
        let tail_result = risp::evaluator::risp_eval(&asts[0], &env).unwrap();

        // Both should give the same result
        prop_assert_eq!(recursive_result, tail_result.clone());

        // And both should match the mathematical formula
        let expected = (n * (n + 1) / 2) as i64;
        prop_assert_eq!(tail_result, Value::Int(expected));
    }
}
