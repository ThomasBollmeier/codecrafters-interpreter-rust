use super::{Interpreter, InterpreterResult};
use crate::interpreter::values::Value;

pub fn echo(values: Vec<Value>) -> InterpreterResult {
    if values.len() != 1 {
        return Interpreter::error("echo expects exactly one argument");
    }

    match &values[0] {
        Value::Str(s) => {
            println!("{}", s);
        }
        _ => return Interpreter::error("echo expects a string argument"),
    }
    Ok(Value::Nil)
}

pub fn hello(_: Vec<Value>) -> InterpreterResult {
    println!("Hello, world!");
    Ok(Value::Nil)
}
