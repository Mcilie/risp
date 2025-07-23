use std::rc::Rc;

use crate::{env::Environment, parser::Expr, value::Value, error::RispError};

pub enum EvalState {
    Value(Value),                    // Final result - we're done
    Continue(Expr, Rc<Environment>), // Keep evaluating this expression in this environment
    Error(RispError),                // Error occurred - propagate up
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Bool(false) => false,
        _ => true,
    }
}

pub fn risp_eval(expr: &Expr, env: &Rc<Environment>) -> Result<Value, RispError> {
    let mut state = EvalState::Continue(expr.clone(), Rc::clone(env));

    loop {
        match state {
            EvalState::Value(v) => return Ok(v),
            EvalState::Error(e) => return Err(e),
            EvalState::Continue(expr, env) => {
                state = risp_eval_step(&expr, &env);
            }
        }
    }
}

pub fn risp_eval_step(expr: &Expr, env: &Rc<Environment>) -> EvalState {
    match expr {
        Expr::Number(n) => EvalState::Value(Value::Int(*n)),
        Expr::Boolean(b) => EvalState::Value(Value::Bool(*b)),
        Expr::Symbol(name) => match env.get(name) {
            Some(value) => EvalState::Value(value),
            None => EvalState::Error(RispError::UnboundVariable(name.clone())),
        },
        Expr::List(elements) => {
            if elements.is_empty() {
                return EvalState::Error(RispError::EmptyList);
            }

            match &elements[0] {
                Expr::Symbol(s) if s == "define" => {
                    if elements.len() < 3 {
                        return EvalState::Error(RispError::InvalidDefine("Define needs var name and at least one body expression".to_string()));
                    }

                    match &elements[1] {
                        Expr::List(signature) => {
                            // Function definition: (define (foo x y) body...)
                            if signature.is_empty() {
                                return EvalState::Error(RispError::InvalidDefine("First arg must be symbol".to_string()));
                            }
                            let func_name = match &signature[0] {
                                Expr::Symbol(name) => name.clone(),
                                _ => return EvalState::Error(RispError::InvalidDefine("First arg must be symbol".to_string())),
                            };

                            let mut params = Vec::new();
                            for param in &signature[1..] {
                                match param {
                                    Expr::Symbol(param_name) => params.push(param_name.clone()),
                                    _ => return EvalState::Error(RispError::InvalidDefine("Params must be symbols".to_string())),
                                }
                            }

                            // Create lambda with body
                            let body = elements[2..].to_vec();
                            let lambda = Value::Lambda {
                                params,
                                body,
                                env: Rc::clone(env),
                            };
                            env.set(func_name, lambda);
                            EvalState::Value(Value::Bool(true))
                        }
                        Expr::Symbol(var_name) => {
                            // Variable definition: (define x value)
                            let body = elements[2..].to_vec();
                            match eval_body(&body, env) {
                                Ok(value) => {
                                    env.set(var_name.clone(), value);
                                    EvalState::Value(Value::Bool(true))
                                }
                                Err(e) => EvalState::Error(e),
                            }
                        }
                        _ => EvalState::Error(RispError::InvalidDefine("First arg must be symbol or list".to_string())),
                    }
                }
                Expr::Symbol(s) if s == "lambda" => {
                    if elements.len() < 3 {
                        return EvalState::Error(RispError::InvalidLambda("Lambda needs parameters and at least one body expression".to_string()));
                    }

                    let params = match &elements[1] {
                        Expr::List(param_list) => {
                            let mut params = Vec::new();
                            for param in param_list {
                                match param {
                                    Expr::Symbol(param_name) => params.push(param_name.clone()),
                                    _ => return EvalState::Error(RispError::InvalidLambda("Lambda parameters must be symbols".to_string())),
                                }
                            }
                            params
                        }
                        _ => return EvalState::Error(RispError::InvalidLambda("First argument to lambda must be a list".to_string())),
                    };

                    let body = elements[2..].to_vec();
                    let lambda = Value::Lambda {
                        params,
                        body,
                        env: Rc::clone(env),
                    };
                    EvalState::Value(lambda)
                }
                Expr::Symbol(s) if s == "if" => {
                    if elements.len() != 3 && elements.len() != 4 {
                        return EvalState::Error(RispError::InvalidIf("if requires 2 or 3 arguments".to_string()));
                    }

                    let condition = match risp_eval(&elements[1], env) {
                        Ok(value) => value,
                        Err(e) => return EvalState::Error(e),
                    };

                    if is_truthy(&condition) {
                        EvalState::Continue(elements[2].clone(), Rc::clone(env))
                    } else if elements.len() == 4 {
                        EvalState::Continue(elements[3].clone(), Rc::clone(env))
                    } else {
                        EvalState::Value(Value::Bool(false))
                    }
                }
                _ => {
                    // Function call
                    let function_expr = &elements[0];
                    let args = &elements[1..];

                    let function_value = match risp_eval(function_expr, env) {
                        Ok(value) => value,
                        Err(e) => return EvalState::Error(e),
                    };

                    match function_value {
                        Value::Proc(proc) => {
                            // Evaluate all arguments
                            let mut arg_values = Vec::new();
                            for arg in args {
                                match risp_eval(arg, env) {
                                    Ok(value) => arg_values.push(value),
                                    Err(e) => return EvalState::Error(e),
                                }
                            }

                            // Call the built-in procedure and handle Result
                            match proc(arg_values.as_slice()) {
                                Ok(value) => EvalState::Value(value),
                                Err(e) => EvalState::Error(e),
                            }
                        }
                        Value::Lambda {
                            params,
                            body,
                            env: captured_env,
                        } => {
                            let arg_len = args.len();
                            if arg_len != params.len() {
                                return EvalState::Error(RispError::ArityMismatch {
                                    expected: params.len(),
                                    got: arg_len,
                                });
                            }

                            // Create new environment for function call
                            let call_env = Environment::extend(captured_env);

                            // Evaluate arguments and bind to parameters
                            for (param, arg) in params.iter().zip(args.iter()) {
                                let arg_value = match risp_eval(arg, env) {
                                    Ok(value) => value,
                                    Err(e) => return EvalState::Error(e),
                                };
                                call_env.set(param.clone(), arg_value);
                            }

                            // Evaluate function body with TCO
                            eval_body_tco(&body, &call_env)
                        }
                        _ => EvalState::Error(RispError::NotCallable(format!("{:?}", function_value))),
                    }
                }
            }
        }
    }
}

// Helper function to evaluate multiple expressions (for function bodies)
pub fn eval_body(exprs: &[Expr], env: &Rc<Environment>) -> Result<Value, RispError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(false)); // Empty body returns false
    }

    // Evaluate all but the last expression for side effects
    for expr in &exprs[..exprs.len() - 1] {
        risp_eval(expr, env)?; // Ignore result, just check for errors
    }

    // Return the value of the last expression (tail call optimization)
    risp_eval(&exprs[exprs.len() - 1], env)
}

// TCO-aware function body evaluation that returns EvalState
fn eval_body_tco(exprs: &[Expr], env: &Rc<Environment>) -> EvalState {
    if exprs.is_empty() {
        return EvalState::Value(Value::Bool(false)); // Empty body returns false
    }

    // Evaluate all but the last expression for side effects
    for expr in &exprs[..exprs.len() - 1] {
        match risp_eval(expr, env) {
            Ok(_) => {}, // Ignore result, just check for errors
            Err(e) => return EvalState::Error(e),
        }
    }

    // Return the last expression as a continuation for proper TCO
    EvalState::Continue(exprs[exprs.len() - 1].clone(), Rc::clone(env))
}

// TCO helper for direct calls without the trampoline
pub fn risp_eval_direct(expr: &Expr, env: &Rc<Environment>) -> Result<Value, RispError> {
    match expr {
        Expr::Number(n) => Ok(Value::Int(*n)),
        Expr::Boolean(b) => Ok(Value::Bool(*b)),
        Expr::Symbol(name) => match env.get(name) {
            Some(value) => Ok(value),
            None => Err(RispError::UnboundVariable(name.clone())),
        },
        Expr::List(elements) => {
            if elements.is_empty() {
                return Err(RispError::EmptyList);
            }

            // For direct evaluation, we handle function calls directly
            let function_value = risp_eval_direct(&elements[0], env)?;
            let args = &elements[1..];

            match function_value {
                Value::Proc(proc) => {
                    let mut arg_values = Vec::new();
                    for arg in args {
                        arg_values.push(risp_eval_direct(arg, env)?);
                    }

                    // Call the built-in procedure and handle Result
                    proc(&arg_values)
                }
                _ => Err(RispError::NotCallable(format!("{:?}", function_value))),
            }
        }
    }
}
