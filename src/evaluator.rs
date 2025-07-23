use std::io::IsTerminal;
use std::rc::Rc;

use crate::{env::Environment, parser::Expr, value::Value};

pub enum EvalState {
    Value(Value),                    // Final result - we're done
    Continue(Expr, Rc<Environment>), // Keep evaluating this expression in this environment
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Bool(false) => false,
        _ => true,
    }
}

pub fn risp_eval(expr: &Expr, env: &Rc<Environment>) -> Value {
    let mut state = EvalState::Continue(expr.clone(), Rc::clone(env));

    loop {
        match state {
            EvalState::Value(v) => return v,
            EvalState::Continue(expr, env) => {
                state = risp_eval_step(&expr, &env);
            }
        }
    }
}
pub fn risp_eval_step(expr: &Expr, env: &Rc<Environment>) -> EvalState {
    match expr {
        // Step 1: Fix number case - wrap in Value::Int
        Expr::Number(n) => EvalState::Value(Value::Int(*n)),

        Expr::Boolean(b) => EvalState::Value(Value::Bool(*b)),
        // Step 2: Fix symbol case - look up in environment
        Expr::Symbol(name) => match env.get(name) {
            Some(value) => EvalState::Value(value),
            None => panic!("Unbound variable: {}", name),
        },

        Expr::List(elements) => {
            if elements.is_empty() {
                panic!("Cannot evaluate empty list");
            }

            let function = &elements[0];
            let args = &elements[1..];

            // Special case: if function is a symbol, handle built-ins and special forms
            if let Expr::Symbol(name) = function {
                match name.as_str() {
                    "define" => {
                        if args.len() < 2 {
                            panic!("Define needs var name and at least one body expression");
                        }

                        match &args[0] {
                            Expr::List(signature) => {
                                let (fname, params) = signature.split_first().unwrap();
                                if !matches!(fname, Expr::Symbol(_)) {
                                    panic!("First arg must be symbol");
                                }
                                let fname_str = match fname {
                                    Expr::Symbol(name) => name.clone(),
                                    _ => panic!("First arg must be symbol"),
                                };
                                let mut param_names: Vec<String> = Vec::new();
                                for param in params {
                                    if !matches!(param, Expr::Symbol(_)) {
                                        panic!("Params must be symbols");
                                    }
                                    //extract name from &expr that is param
                                    let name = match param {
                                        Expr::Symbol(name) => name.clone(),
                                        _ => panic!("Params must be symbols"),
                                    };
                                    param_names.push(name);
                                }
                                let lambda_value = Value::Lambda {
                                    params: param_names,
                                    body: args[1..].to_vec(),
                                    env: Rc::clone(env),
                                };
                                env.set(fname_str.clone(), lambda_value.clone());
                                EvalState::Value(lambda_value)
                            }
                            Expr::Symbol(name) => {
                                // let variable_name = match &args[0] {
                                //     Expr::Symbol(name) => name.clone(),
                                //     _ => panic!("First argument to define must be a symbol"),
                                // };
                                let value = risp_eval(&args[1], env);

                                env.set(name.clone(), value.clone());
                                EvalState::Value(value)
                            }
                            _ => panic!("First arg must be symbol or list"),
                        }
                    }
                    "lambda" => {
                        if args.len() < 2 {
                            panic!(
                                "lambda requires parameter list and at least one body expression"
                            );
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
                        EvalState::Value(Value::Lambda {
                            params,
                            body: args[1..].to_vec(),
                            env: Rc::clone(env),
                        })
                    }
                    "if" => {
                        if args.len() < 2 || args.len() > 3 {
                            panic!("if requires 2 or 3 arguments");
                        }

                        // Evaluate condition
                        let condition = risp_eval(&args[0], env);

                        if is_truthy(&condition) {
                            // Condition is true - evaluate "then" branch (TAIL CALL)
                            EvalState::Continue(args[1].clone(), Rc::clone(env))
                        } else {
                            // Condition is false
                            if args.len() == 3 {
                                // Has "else" branch - evaluate it (TAIL CALL)
                                EvalState::Continue(args[2].clone(), Rc::clone(env))
                            } else {
                                // No "else" branch - return #f (DIRECT VALUE)
                                EvalState::Value(Value::Bool(false))
                            }
                        }
                    }
                    _ => match env.get(name) {
                        Some(value) => match value {
                            Value::Proc(proc) => {
                                let arg_values: Vec<Value> =
                                    args.iter().map(|arg| risp_eval(arg, env)).collect();
                                EvalState::Value(proc(arg_values.as_slice()))
                            }
                            Value::Lambda {
                                params,
                                body,
                                env: captured_env,
                            } => {
                                let arg_len = args.len();
                                if arg_len != params.len() {
                                    panic!(
                                        "Lambda expects {} arguments, got {}",
                                        params.len(),
                                        arg_len
                                    );
                                }
                                let evaled_args: Vec<Value> =
                                    args.iter().map(|arg| risp_eval(arg, env)).collect();
                                let child_env = Environment::extend(captured_env);
                                for (param, arg_value) in params.iter().zip(evaled_args.iter()) {
                                    child_env.set(param.clone(), arg_value.clone());
                                }
                                eval_body(&body, &child_env)
                            }
                            _ => panic!("Cannot call non-function value: {:?}", value),
                        },
                        None => panic!("Unbound variable: {}", name),
                    },
                }
            } else {
                // Not a symbol - evaluate the function and handle lambda calls
                let function_value = risp_eval(function, env);
                match function_value {
                    Value::Lambda {
                        params,
                        body,
                        env: captured_env,
                    } => {
                        // Check arity
                        if params.len() != args.len() {
                            panic!(
                                "Lambda expects {} arguments, got {}",
                                params.len(),
                                args.len()
                            );
                        }

                        // Evaluate all arguments
                        let arg_values: Vec<Value> =
                            args.iter().map(|arg| risp_eval(arg, env)).collect();

                        // Create child environment extending the captured environment
                        let child_env = Environment::extend(captured_env);

                        // Bind parameters to argument values
                        for (param, arg_value) in params.iter().zip(arg_values.iter()) {
                            child_env.set(param.clone(), arg_value.clone());
                        }

                        // Evaluate the lambda body in the child environment (TAIL CALL)
                        eval_body(&body, &child_env)
                    }
                    Value::Proc(proc) => {
                        // Evaluate all arguments
                        let arg_values: Vec<Value> =
                            args.iter().map(|arg| risp_eval(arg, env)).collect();

                        // Call the built-in procedure (DIRECT VALUE)
                        EvalState::Value(proc(&arg_values))
                    }
                    _ => panic!("Cannot call non-function value: {:?}", function_value),
                }
            }
        }
    }
}

// Helper function to evaluate multi-expression lambda bodies with TCO
fn eval_body(exprs: &[Expr], env: &Rc<Environment>) -> EvalState {
    if exprs.is_empty() {
        return EvalState::Value(Value::Int(0)); // Default value for empty body
    }

    // Evaluate all expressions except the last one normally
    for expr in &exprs[..exprs.len() - 1] {
        risp_eval(expr, env); // Side effects only, ignore result
    }

    // Return the last expression as a tail call for TCO
    EvalState::Continue(exprs[exprs.len() - 1].clone(), Rc::clone(env))
}

pub fn risp_eval_program(exprs: &[Expr], env: &Rc<Environment>) -> Value {
    if exprs.is_empty() {
        return Value::Int(0); // Default value for empty program
    }

    let mut result = Value::Int(0);
    for expr in exprs {
        result = risp_eval(expr, env);
    }
    result
}
