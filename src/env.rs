use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type Frame = HashMap<String, Value>;

// Helper function for truthiness (same as in evaluator)
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Bool(false) => false,
        _ => true,
    }
}

#[derive(Clone)]
pub struct Environment {
    frame: Rc<RefCell<Frame>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new_root() -> Rc<Self> {
        let hash_map = HashMap::new();
        let ref_cell = RefCell::new(hash_map);
        let rc_ref_cell = Rc::new(ref_cell);

        let new_env = Rc::new(Environment {
            frame: rc_ref_cell,
            parent: None,
        });

        // Add the + built-in procedure
        let plus_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            let sum = args
                .iter()
                .map(|arg| match arg {
                    Value::Int(n) => *n,
                    _ => panic!("+ requires integers"),
                })
                .sum::<i32>();
            Value::Int(sum)
        }));
        new_env.set("+".to_string(), plus_proc);

        // Add the - built-in procedure with support for 1 arg and many arg cases
        let minus_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            if args.is_empty() {
                panic!("- requires at least 1 argument");
            }
            if args.len() == 1 {
                // Unary negation
                match &args[0] {
                    Value::Int(n) => Value::Int(-n),
                    _ => panic!("- requires integer"),
                }
            } else {
                let first = match &args[0] {
                    Value::Int(n) => n,
                    _ => panic!("- requires integers"),
                };
                let rest_sum = args[1..]
                    .iter()
                    .map(|arg| match arg {
                        Value::Int(n) => n,
                        _ => panic!("- requires integers"),
                    })
                    .sum::<i32>();
                Value::Int(first - rest_sum)
            }
        }));
        new_env.set("-".to_string(), minus_proc);

        // Add the * built-in procedure
        let mult_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            let product = args
                .iter()
                .map(|arg| match arg {
                    Value::Int(n) => *n,
                    _ => panic!("* requires integers"),
                })
                .product::<i32>();
            Value::Int(product)
        }));
        new_env.set("*".to_string(), mult_proc);

        // Add the / built-in procedure
        let div_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            if args.is_empty() {
                panic!("/ requires at least 1 argument");
            }
            if args.len() == 1 {
                // Unary division: reciprocal
                match &args[0] {
                    Value::Int(n) => {
                        if *n == 0 {
                            panic!("Division by zero");
                        }
                        Value::Int(1 / n)
                    }
                    _ => panic!("/ requires integer"),
                }
            } else {
                // N-ary division
                let first = match &args[0] {
                    Value::Int(n) => *n,
                    _ => panic!("/ requires integers"),
                };
                let mut result = first;

                for arg in &args[1..] {
                    let divisor = match arg {
                        Value::Int(n) => *n,
                        _ => panic!("/ requires integers"),
                    };
                    if divisor == 0 {
                        panic!("Division by zero");
                    }
                    result = result / divisor;
                }
                Value::Int(result)
            }
        }));
        new_env.set("/".to_string(), div_proc);

        // Add the = built-in procedure
        let eq_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            if args.len() < 2 {
                panic!("= requires at least 2 arguments");
            }
            let first_int = match &args[0] {
                Value::Int(n) => *n,
                _ => panic!("= requires integers"),
            };

            let all_equal = args[1..].iter().all(|arg| match arg {
                Value::Int(n) => *n == first_int,
                _ => panic!("= requires integers"),
            });
            Value::Bool(all_equal)
        }));
        new_env.set("=".to_string(), eq_proc);

        // Add the < built-in procedure
        let lt_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            if args.len() != 2 {
                panic!("< requires exactly 2 arguments");
            }
            let (left_int, right_int) = match (&args[0], &args[1]) {
                (Value::Int(l), Value::Int(r)) => (*l, *r),
                _ => panic!("< requires integers"),
            };
            Value::Bool(left_int < right_int)
        }));
        new_env.set("<".to_string(), lt_proc);

        // Add the > built-in procedure
        let gt_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            if args.len() != 2 {
                panic!("> requires exactly 2 arguments");
            }
            let (left_int, right_int) = match (&args[0], &args[1]) {
                (Value::Int(l), Value::Int(r)) => (*l, *r),
                _ => panic!("> requires integers"),
            };
            Value::Bool(left_int > right_int)
        }));
        new_env.set(">".to_string(), gt_proc);

        // Add the and built-in procedure
        let and_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            for arg in args {
                if !is_truthy(arg) {
                    return Value::Bool(false);
                }
            }
            Value::Bool(true)
        }));
        new_env.set("and".to_string(), and_proc);

        // Add the or built-in procedure
        let or_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            for arg in args {
                if is_truthy(arg) {
                    return Value::Bool(true);
                }
            }
            Value::Bool(false)
        }));
        new_env.set("or".to_string(), or_proc);

        // Add the not built-in procedure
        let not_proc = Value::Proc(Rc::new(|args: &[Value]| -> Value {
            if args.len() != 1 {
                panic!("not expects 1 argument");
            }
            Value::Bool(!is_truthy(&args[0]))
        }));
        new_env.set("not".to_string(), not_proc);

        new_env
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        //check current frame
        if let Some(value) = self.frame.borrow().get(name) {
            return Some(value.clone());
        }

        if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: String, value: Value) {
        self.frame.borrow_mut().insert(name, value);
    }

    pub fn extend(parent: Rc<Environment>) -> Rc<Self> {
        Rc::new(Environment {
            frame: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(parent),
        })
    }
}
