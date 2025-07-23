use crate::env::Environment;
use crate::error::RispError;
use crate::parser::Expr;
use std::rc::Rc;

pub enum Value {
    Int(i64),
    Bool(bool),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Rc<Environment>,
    },
    Proc(Rc<dyn Fn(&[Value]) -> Result<Value, RispError>>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Lambda {
                params: _,
                body: _,
                env: _,
            } => write!(f, "<lambda>"),
            Value::Proc(_p) => write!(f, "<proc>"),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            Value::Int(n) => Value::Int(*n),
            Value::Bool(b) => Value::Bool(*b),
            Value::Lambda { params, body, env } => Value::Lambda {
                params: params.clone(),
                body: body.clone(),
                env: env.clone(),
            },
            Value::Proc(p) => Value::Proc(p.clone()),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Lambda {
                params: _,
                body: _,
                env: _,
            } => write!(f, "<lambda>"),
            Value::Proc(_p) => write!(f, "<proc>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (
                Value::Lambda {
                    params: _a_params,
                    body: _a_body,
                    env: _a_env,
                },
                Value::Lambda {
                    params: _b_params,
                    body: _b_body,
                    env: _b_env,
                },
            ) => false,
            (Value::Proc(_a), Value::Proc(_b)) => false,
            _ => false,
        }
    }
}
