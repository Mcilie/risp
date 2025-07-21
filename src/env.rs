use crate::value::Value;
use std::collections::HashMap;

pub struct Environment {
    env: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            env: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.env.get(name)
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.env.insert(name, value);
    }
}
