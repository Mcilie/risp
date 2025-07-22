use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type Frame = HashMap<String, Value>;

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
        Rc::new(Environment {
            frame: rc_ref_cell,
            parent: None,
        })
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
