use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstType};
use crate::interpreter::{Interpreter, InterpreterResult};
use std::cell::RefCell;
use std::collections::HashMap;
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
    ClassDef(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Return(Box<Value>),
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
            Value::ClassDef(class) => write!(f, "{}", class.get_name()),
            Value::Instance(instance) => {
                write!(f, "{} instance", instance.borrow().class.get_name())
            }
            Value::Return(value) => write!(f, "return value: {}", value),
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

    pub fn bind(&self, instance: Rc<RefCell<Instance>>, class: Rc<Class>) -> UserFunction {
        let interpreter = self.interpreter.copy();
        let env = interpreter.get_env();
        
        env.borrow_mut()
            .set_value("this".to_string(), Value::Instance(instance.clone()));
        
        if let Some(super_class) = &class.super_class {
            let super_inst = instance.borrow().super_instance(super_class.clone());
            env.borrow_mut().set_value(
                "super".to_string(),
                Value::Instance(Rc::new(RefCell::new(super_inst))),
            );
        }

        UserFunction {
            interpreter: Rc::new(interpreter),
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
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

        let body = match &self.body.as_ref() {
            Ast::NonTerminal(ast_node) if ast_node.get_type() == &AstType::Block => ast_node,
            _ => return Interpreter::error("Function body must be a block"),
        };

        let result = interpreter.eval_block(body)?;
        if let Value::Return(value) = result {
            Ok(*value)
        } else {
            Ok(result)
        }
    }
}

#[derive(Clone)]
pub struct Class {
    name: String,
    super_class: Option<Rc<Class>>,
    methods: Vec<UserFunction>,
}

impl Class {
    pub fn new(name: String, super_class: Option<Rc<Class>>, methods: Vec<UserFunction>) -> Class {
        Class {
            name,
            super_class,
            methods,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_methods(&self) -> &Vec<UserFunction> {
        &self.methods
    }

    pub fn get_method<'a>(
        class: &'a Rc<Class>,
        name: &str,
    ) -> Option<(&'a UserFunction, Rc<Class>)> {
        let method = class
            .methods
            .iter()
            .find(|&method| method.get_name() == name);
        match method {
            Some(m) => Some((m, class.clone())),
            None => {
                if let Some(super_class) = &class.super_class {
                    Class::get_method(super_class, name)
                } else {
                    None
                }
            }
        }
    }

    pub fn add_method(&mut self, method: UserFunction) {
        self.methods.push(method);
    }
}

pub fn class_call(class: Rc<Class>, args: Vec<Value>) -> InterpreterResult {
    let instance = Rc::new(RefCell::new(Instance::new(class.clone())));
    if let Some((init_method, _)) = Class::get_method(&class, "init") {
        let method = init_method.bind(Rc::clone(&instance), class.clone());
        method.call(args)?;
    }
    Ok(Value::Instance(instance))
}

#[derive(Clone)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

pub type InstanceRef = Rc<RefCell<Instance>>;

impl Instance {
    pub fn new(class: Rc<Class>) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn super_instance(&self, super_class: Rc<Class>) -> Instance {
        Instance {
            class: super_class,
            fields: self.fields.clone(),
        }
    }

    pub fn set_field(&mut self, name: String, value: Value) {
        self.fields.insert(name, value);
    }

    pub fn get_member(instance_ref: InstanceRef, name: &str) -> Option<Value> {
        match instance_ref.borrow().fields.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some((func, class)) = Class::get_method(&instance_ref.borrow().class, name) {
                    let method = func.bind(Rc::clone(&instance_ref), class);
                    return Some(Value::UserFunc(method.clone()));
                }
                None
            }
        }
    }

    pub fn call_method(
        instance_ref: InstanceRef,
        name: &str,
        args: Vec<Value>,
    ) -> InterpreterResult {
        let method = Self::get_member(instance_ref, name).ok_or(LoxError::new_in_eval_ctx(
            format!("unknown member {}", name),
        ))?;

        let method = match method {
            Value::UserFunc(func) => func,
            _ => return Interpreter::error("only functions can be called"),
        };

        method.call(args)
    }
}
