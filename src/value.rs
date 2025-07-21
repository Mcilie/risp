#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
        }
    }
}
