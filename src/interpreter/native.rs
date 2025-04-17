use super::{Interpreter, InterpreterResult};
use crate::interpreter::values::Value;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn clock(_: Vec<Value>) -> InterpreterResult {
    let now = SystemTime::now();
    
    match now.duration_since(UNIX_EPOCH) {
        Ok(duration) => Ok(Value::Number(duration.as_secs() as f64)),
        Err(_) => Interpreter::error("Cannot get current time"),
    }
}
