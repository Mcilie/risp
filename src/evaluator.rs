use std::io::IsTerminal;
use std::rc::Rc;

use crate::{env::Environment, parser::Expr, value::Value};

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Bool(false) => false,
        _ => true,
    }
}

pub fn risp_eval(expr: &Expr, env: &Rc<Environment>) -> Value {
    match expr {
        // Step 1: Fix number case - wrap in Value::Int
        Expr::Number(n) => Value::Int(*n),

        Expr::Boolean(b) => Value::Bool(*b),
        // Step 2: Fix symbol case - look up in environment
        Expr::Symbol(name) => match env.get(name) {
            Some(value) => value,
            None => panic!("Unbound variable: {}", name),
        },

        Expr::List(elements) => {
            if elements.is_empty() {
                panic!("Cannot evaluate empty list");
            }

            let function = &elements[0];
            let args = &elements[1..];

            match function {
                Expr::Symbol(name) => match name.as_str() {
                    "define" => {
                        if args.len() != 2 {
                            panic!("Define needs var name and one other arg");
                        }
                        let variable_name = match &args[0] {
                            Expr::Symbol(name) => name.clone(),
                            _ => panic!("First argument to define must be a symbol"),
                        };
                        let value = risp_eval(&args[1], env);

                        env.set(variable_name, value.clone());
                        value
                    }
                    "lambda" => {
                        if args.len() != 2 {
                            panic!("lambda requires 2 arguments");
                        }
                        let params = match &args[0] {
                            Expr::List(params) => {
                                let mut params_list = Vec::new();
                                for param in params {
                                    match param {
                                        Expr::Symbol(name) => params_list.push(name.clone()),
                                        _ => panic!("Lambda parameters must be symbols"),
                                    }
                                }
                                params_list
                            }
                            _ => panic!("First argument to lambda must be a list"),
                        };
                        return Value::Lambda {
                            params,
                            body: args[1].clone(),
                            env: Rc::clone(env),
                        };
                    }
                    "if" => {
                        if args.len() < 2 || args.len() > 3 {
                            panic!("if requires 2 or 3 arguments");
                        }

                        // Evaluate condition
                        let condition = risp_eval(&args[0], env);

                        if is_truthy(&condition) {
                            // Condition is true - evaluate "then" branch
                            risp_eval(&args[1], env)
                        } else {
                            // Condition is false
                            if args.len() == 3 {
                                // Has "else" branch - evaluate it
                                risp_eval(&args[2], env)
                            } else {
                                // No "else" branch - return #f
                                Value::Bool(false)
                            }
                        }
                    }
                    "and" => {
                        for arg in args {
                            let evaled = risp_eval(arg, env); // Evaluate one at a time
                            if !is_truthy(&evaled) {
                                return Value::Bool(false); // Stop immediately on first false
                            }
                        }
                        Value::Bool(true) // All were truthy
                    }
                    "or" => {
                        for arg in args {
                            let evaled = risp_eval(arg, env); // Evaluate one at a time
                            if is_truthy(&evaled) {
                                return Value::Bool(true); // Stop immediately on first true
                            }
                        }
                        Value::Bool(false) // All were false
                    }
                    "not" => {
                        if args.len() != 1 {
                            panic!("not expects 1 argument")
                        }
                        return Value::Bool(!is_truthy(&risp_eval(&args[0], env)));
                    }
                    "=" => {
                        if args.len() < 2 {
                            panic!("= requires at least 2 arguments");
                        }
                        let first_val = risp_eval(&args[0], env);
                        let first_int = match first_val {
                            Value::Int(n) => n,
                            _ => panic!("= requires integers"),
                        };

                        let all_equal = args[1..].iter().all(|arg| {
                            let val = risp_eval(arg, env);
                            match val {
                                Value::Int(n) => n == first_int,
                                _ => panic!("= requires integers"),
                            }
                        });
                        Value::Bool(all_equal)
                    }
                    "<" => {
                        if args.len() != 2 {
                            panic!("< requires exactly 2 arguments");
                        }
                        let left = risp_eval(&args[0], env);
                        let right = risp_eval(&args[1], env);
                        let (left_int, right_int) = match (left, right) {
                            (Value::Int(l), Value::Int(r)) => (l, r),
                            _ => panic!("< requires integers"),
                        };
                        Value::Bool(left_int < right_int)
                    }
                    ">" => {
                        if args.len() != 2 {
                            panic!("> requires exactly 2 arguments");
                        }
                        let left = risp_eval(&args[0], env);
                        let right = risp_eval(&args[1], env);
                        let (left_int, right_int) = match (left, right) {
                            (Value::Int(l), Value::Int(r)) => (l, r),
                            _ => panic!("> requires integers"),
                        };
                        Value::Bool(left_int > right_int)
                    }

                    // Step 3: Fix addition - need helper function to extract integers
                    "+" => {
                        let sum = args
                            .iter()
                            .map(|arg| risp_eval(arg, env)) // Pass env to each call
                            .map(|val| match val {
                                // Extract integer from Value
                                Value::Int(n) => n,
                                _ => panic!("+ requires integers"),
                            })
                            .sum::<i32>();
                        Value::Int(sum) // Wrap result back in Value::Int
                    }
                    "-" => {
                        if args.is_empty() {
                            panic!("- requires at least 1 argument");
                        }
                        if args.len() == 1 {
                            // Unary negation
                            let val = risp_eval(&args[0], env);
                            match val {
                                Value::Int(n) => Value::Int(-n),
                                _ => panic!("- requires integer"),
                            }
                        } else {
                            let first = risp_eval(&args[0], env);
                            let first_int = match first {
                                Value::Int(n) => n,
                                _ => panic!("- requires integers"),
                            };
                            let rest_sum = args[1..]
                                .iter()
                                .map(|arg| risp_eval(arg, env))
                                .map(|val| match val {
                                    Value::Int(n) => n,
                                    _ => panic!("- requires integers"),
                                })
                                .sum::<i32>();
                            Value::Int(first_int - rest_sum)
                        }
                    }
                    "*" => {
                        let product = args
                            .iter()
                            .map(|arg| risp_eval(arg, env))
                            .map(|val| match val {
                                Value::Int(n) => n,
                                _ => panic!("* requires integers"),
                            })
                            .product::<i32>();
                        Value::Int(product)
                    }
                    "/" => {
                        if args.is_empty() {
                            panic!("/ requires at least 1 argument");
                        }
                        if args.len() == 1 {
                            // Unary division: reciprocal (/ 8) = 1/8
                            let val = risp_eval(&args[0], env);
                            match val {
                                Value::Int(n) => {
                                    if n == 0 {
                                        panic!("Division by zero");
                                    }
                                    Value::Int(1 / n) // Note: integer division, so 1/8 = 0
                                }
                                _ => panic!("/ requires integer"),
                            }
                        } else {
                            // N-ary division: (/ 8 2 2) = 8 / 2 / 2 = 2
                            let first = risp_eval(&args[0], env);
                            let mut result = match first {
                                Value::Int(n) => n,
                                _ => panic!("/ requires integers"),
                            };

                            for arg in &args[1..] {
                                let val = risp_eval(arg, env);
                                let divisor = match val {
                                    Value::Int(n) => n,
                                    _ => panic!("/ requires integers"),
                                };
                                if divisor == 0 {
                                    panic!("Division by zero");
                                }
                                result = result / divisor;
                            }
                            Value::Int(result)
                        }
                    }
                    _ => panic!("Unknown function: {}", name),
                },
                _ => panic!("First element of list must be a function"),
            }
        }
    }
}
