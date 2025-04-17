use crate::interpreter::InterpreterResult;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    Str(String),
    Native(NativeFunction),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Number(num) => write!(f, "{}", num),
            Value::Str(s) => write!(f, "{}", s),
            Value::Native(_) => write!(f, "<native fn>"),
        }
    }
}

pub trait Callable: Clone {
    fn call(&self, args: Vec<Value>) -> InterpreterResult;
}

#[derive(Clone)]
pub struct NativeFunction {
    callable: Rc<dyn Fn(Vec<Value>) -> InterpreterResult>,
}

impl NativeFunction {
    pub fn new(callable: Rc<dyn Fn(Vec<Value>) -> InterpreterResult>) -> NativeFunction {
        NativeFunction { callable }
    }

    pub fn call(&self, args: Vec<Value>) -> InterpreterResult {
        (self.callable)(args)
    }
}
