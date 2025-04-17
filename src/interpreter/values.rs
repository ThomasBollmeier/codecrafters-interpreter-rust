use crate::frontend::ast::Ast;
use crate::interpreter::{Interpreter, InterpreterResult};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    Str(String),
    NativeFunc(NativeFunction),
    UserFunc(UserFunction),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Number(num) => write!(f, "{}", num),
            Value::Str(s) => write!(f, "{}", s),
            Value::NativeFunc(_) => write!(f, "<native fn>"),
            Value::UserFunc(func) => write!(f, "<fn {}>", func.get_name()),
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

#[derive(Clone)]
pub struct UserFunction {
    interpreter: Rc<Interpreter>,
    name: String,
    params: Vec<String>,
    body: Rc<Ast>,
}

impl UserFunction {
    pub fn new(
        interpreter: Rc<Interpreter>,
        name: String,
        params: Vec<String>,
        body: Rc<Ast>,
    ) -> UserFunction {
        UserFunction {
            interpreter,
            name,
            params,
            body,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}

impl Callable for UserFunction {
    fn call(&self, args: Vec<Value>) -> InterpreterResult {
        if args.len() != self.params.len() {
            return Interpreter::error(&format!(
                "expected {} arguments, but got {}",
                self.params.len(),
                args.len()
            ));
        }

        let interpreter = self.interpreter.new_child();
        let env = interpreter.get_env();
        for (param, arg) in self.params.iter().zip(args) {
            env.borrow_mut().set_value(param.clone(), arg);
        }

        self.interpreter.eval_ast(&self.body.clone())
    }
}