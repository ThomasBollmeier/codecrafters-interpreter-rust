use crate::interpreter::values::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
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

    pub fn update_value(&mut self, name: String, value: Value) -> bool {
        if let Entry::Occupied(mut e) = self.values.entry(name.clone()) {
            e.insert(value);
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().update_value(name, value);
        }
        false
    }
    
    pub fn update_value_at_level(env: EnvRef, name: String, value: Value, scope_level: i32) -> bool {
        let mut level = 0;
        let mut current_env = env;
        
        while level < scope_level {
            let parent = {
                let parent = &current_env.borrow().parent;
                parent.clone()
            };
            if let Some(parent) = parent {
                level += 1;
                current_env = parent.clone();
            } else {
                return false;
            }
        }
        
        let ret = current_env.borrow_mut().update_value(name, value);
        ret
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

    pub fn get_value_at_level(env: EnvRef, name: &str, scope_level: i32) -> Option<Value> {
        let mut level = 0;
        let mut current_env = env;
        
        while level < scope_level {
            let parent = {
                let parent = &current_env.borrow().parent;
                parent.clone()
            };
            if let Some(parent) = parent {
                level += 1;
                current_env = parent.clone();
            } else {
                return None;
            }
        }
        
        let value_opt = current_env.borrow().get_value(name);
        
        value_opt
    }
}
