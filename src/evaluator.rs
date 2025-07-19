use crate::parser::Expr;

pub fn risp_eval(expr: &Expr) -> i32 {
    match expr {
        Expr::Number(n) => *n,

        Expr::Symbol(name) => {
            panic!("Unknown symbol: {}", name);
        }

        Expr::List(elements) => {
            if elements.is_empty() {
                panic!("Cannot evaluate empty list");
            }

            let function = &elements[0];
            let args = &elements[1..];

            match function {
                Expr::Symbol(name) => match name.as_str() {
                    "+" => args.iter().map(risp_eval).sum(),
                    "-" => {
                        if args.is_empty() {
                            panic!("- requires at least 1 argument");
                        }
                        if args.len() == 1 {
                            -risp_eval(&args[0]) // Unary negation
                        } else {
                            let first = risp_eval(&args[0]);
                            let rest: i32 = args[1..].iter().map(risp_eval).sum();
                            first - rest
                        }
                    }
                    "*" => args.iter().map(risp_eval).product(),
                    "/" => {
                        if args.len() != 2 {
                            panic!("/ requires exactly 2 arguments");
                        }
                        let right_arg = risp_eval(&args[1]);
                        if right_arg == 0 {
                            panic!("Division by zero");
                        }
                        risp_eval(&args[0]) / right_arg
                    }
                    _ => panic!("Unknown function: {}", name),
                },
                _ => panic!("First element of list must be a function"),
            }
        }
    }
}
