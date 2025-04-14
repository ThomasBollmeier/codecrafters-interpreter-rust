use crate::interpreter::values::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Env {
    parent: Option<EnvRef>,
    values: HashMap<String, Value>,
}

pub type EnvRef = Rc<RefCell<Env>>;

impl Env {
    pub fn new_ref(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Env {
            parent,
            values: HashMap::new(),
        }))
    }

    pub fn set_value(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get_value(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some(parent) = &self.parent {
                    parent.borrow().get_value(name)
                } else {
                    None
                }
            }
        }
    }
}
