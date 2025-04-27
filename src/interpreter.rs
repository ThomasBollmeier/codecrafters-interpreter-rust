use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstNode, AstType, AstValue};
use crate::frontend::parser;
use crate::frontend::tokens::{Literal, TokenType};
use crate::interpreter::env::{Env, EnvRef};
use crate::interpreter::values::{class_call, Callable, Class, Instance, UserFunction, Value};
use std::cell::RefCell;
use std::rc::Rc;
use values::NativeFunction;

mod env;
mod native;
pub mod values;

pub type InterpreterResult = Result<Value, LoxError>;

pub struct Interpreter {
    env: EnvRef,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let env = Env::new_ref(None);
        Self::add_native_functions(env.clone());

        Interpreter { env }
    }

    fn add_native_functions(env: EnvRef) {
        Self::add_native_function(env.clone(), "clock", Rc::new(native::clock));
    }

    fn add_native_function(
        env: EnvRef,
        name: &str,
        callable: Rc<dyn Fn(Vec<Value>) -> InterpreterResult>,
    ) {
        let native_fn = NativeFunction::new(callable);
        env.borrow_mut()
            .set_value(name.to_string(), Value::NativeFunc(native_fn));
    }

    fn new_child(&self) -> Interpreter {
        Interpreter {
            env: Env::new_ref(Some(self.env.clone())),
        }
    }

    pub fn copy(&self) -> Interpreter {
        let env = self.env.borrow().clone();
        Interpreter {
            env: Rc::new(RefCell::new(env)),
        }
    }

    pub fn run(&self, code: String) -> Result<(), LoxError> {
        let ast = parser::parse_program(&code)?;
        self.eval_ast(&ast).map(|_| {})
    }

    pub fn eval(&self, code: String) -> InterpreterResult {
        let ast = parser::parse_expression(&code)?;
        self.eval_ast(&ast)
    }

    pub fn get_env(&self) -> EnvRef {
        self.env.clone()
    }

    fn eval_ast(&self, ast: &Ast) -> InterpreterResult {
        match ast {
            Ast::Terminal(token) => match token.token_type {
                TokenType::Nil => Ok(Value::Nil),
                TokenType::True => Ok(Value::Boolean(true)),
                TokenType::False => Ok(Value::Boolean(false)),
                TokenType::Number => match token.literal {
                    Literal::Number(num) => Ok(Value::Number(num)),
                    _ => Self::error("invalid literal value"),
                },
                TokenType::Str => match &token.literal {
                    Literal::Str(s) => Ok(Value::Str(s.clone())),
                    _ => Self::error("invalid literal value"),
                },
                _ => Self::error("unsupported token"),
            },
            Ast::NonTerminal(ast_node) => match ast_node.get_type() {
                AstType::Program => self.eval_program(ast_node),
                AstType::VarDecl => self.eval_var_decl(ast_node),
                AstType::FunDecl => self.eval_fun_decl(ast_node),
                AstType::ClassDecl => self.eval_class_decl(ast_node),
                AstType::Block => self.eval_block(ast_node),
                AstType::IfStmt => self.eval_if_stmt(ast_node),
                AstType::WhileStmt => self.eval_while_stmt(ast_node),
                AstType::ForStmt => self.eval_for_stmt(ast_node),
                AstType::PrintStmt => self.eval_print_stmt(ast_node),
                AstType::ReturnStmt => self.eval_return_stmt(ast_node),
                AstType::ExprStmt => self.eval_expr_stmt(ast_node),
                AstType::Group => {
                    let expr = &ast_node.get_children()[1];
                    self.eval_ast(expr)
                }
                AstType::Unary => self.eval_unary(ast_node),
                AstType::Binary => self.eval_binary(ast_node),
                AstType::Assignment => self.eval_assignment(ast_node),
                AstType::Disjunction => self.eval_disjunction(ast_node),
                AstType::Conjunction => self.eval_conjunction(ast_node),
                AstType::Call => self.eval_call(ast_node),
                AstType::VarRef => self.eval_var_ref(ast_node),
            },
        }
    }

    fn eval_program(&self, ast_node: &AstNode) -> InterpreterResult {
        let mut result = Ok(Value::Nil);
        for stmt in ast_node.get_children() {
            result = Ok(self.eval_ast(stmt)?);
        }

        result
    }

    fn eval_var_decl(&self, ast_node: &AstNode) -> InterpreterResult {
        let var_name = match ast_node.get_value() {
            Some(AstValue::Str(name)) => name.clone(),
            _ => return Self::error("invalid ast value"),
        };

        let children = ast_node.get_children();
        let init_value = if children.len() == 5 {
            self.eval_ast(&children[3])?
        } else {
            Value::Nil
        };
        self.env.borrow_mut().set_value(var_name, init_value);

        Ok(Value::Nil)
    }

    fn eval_fun_decl(&self, ast_node: &AstNode) -> InterpreterResult {
        let function = self.create_user_function(ast_node)?;

        self.env
            .borrow_mut()
            .set_value(function.get_name().to_string(), Value::UserFunc(function));

        Ok(Value::Nil)
    }

    fn create_user_function(&self, ast_node: &AstNode) -> Result<UserFunction, LoxError> {
        let name = ast_node
            .get_value_str()
            .ok_or(LoxError::new_in_eval_ctx(String::from("invalid ast value")))?;

        let children = ast_node.get_children();
        let mut params = Vec::new();
        let mut body = None;
        for child in children {
            match child {
                Ast::Terminal(token) => {
                    if token.token_type == TokenType::Identifier {
                        params.push(token.lexeme.clone());
                    }
                }
                Ast::NonTerminal(ast_node) if ast_node.get_type() == &AstType::Block => {
                    body = Some(child);
                }
                _ => return Err(LoxError::new_in_eval_ctx(String::from("invalid ast value"))),
            }
        }

        let body = body.unwrap().clone();

        let function = UserFunction::new(
            Rc::new(self.new_child()),
            name.clone(),
            params,
            Rc::new(body),
        );

        Ok(function)
    }

    fn eval_class_decl(&self, ast_node: &AstNode) -> InterpreterResult {
        let class_name = ast_node
            .get_value_str()
            .ok_or(LoxError::new_in_eval_ctx("invalid class name".to_string()))?;

        let mut methods = Vec::new();
        for child in ast_node.get_children() {
            if let Ast::NonTerminal(ast_node) = child {
                if ast_node.get_type() == &AstType::FunDecl {
                    methods.push(self.create_user_function(ast_node)?);
                }
            }
        }

        let class = Rc::new(Class::new(class_name, methods));

        self.env
            .borrow_mut()
            .set_value(class.get_name().to_string(), Value::ClassDef(class));

        Ok(Value::Nil)
    }

    fn eval_block(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children();
        let stmts = &children[1..children.len() - 1]; // first + last child are braces
        let interpreter = self.new_child();

        for stmt in stmts {
            let value = interpreter.eval_ast(stmt)?;
            if let Value::Return(_) = &value {
                return Ok(value);
            }
        }

        Ok(Value::Nil)
    }

    fn eval_if_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children(); // 'if' '(' cond ')' then_branch ('else' else_branch)?
        let cond_value = self.eval_ast(&children[2])?;
        let value = if Self::is_truthy(&cond_value) {
            self.eval_ast(&children[4])?
        } else if children.len() == 7 {
            self.eval_ast(&children[6])?
        } else {
            Value::Nil
        };

        if let Value::Return(_) = &value {
            return Ok(value);
        }

        Ok(Value::Nil)
    }

    fn eval_while_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children();
        let condition = &children[0];
        let stmt = &children[1];
        loop {
            let cond_value = self.eval_ast(condition)?;
            if !Self::is_truthy(&cond_value) {
                break;
            }
            let value = self.eval_ast(stmt)?;
            if let Value::Return(_) = &value {
                return Ok(value);
            }
        }

        Ok(Value::Nil)
    }

    fn eval_for_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        let mut initializer: Option<&Ast> = None;
        let mut condition: Option<&Ast> = None;
        let mut increment: Option<&Ast> = None;
        let mut statement: Option<&Ast> = None;

        let children = ast_node.get_children();
        for child in children {
            match child.get_label().as_str() {
                "initializer" => {
                    initializer = Some(child);
                }
                "condition" => {
                    condition = Some(child);
                }
                "increment" => {
                    increment = Some(child);
                }
                _ => {
                    statement = Some(child);
                }
            }
        }

        let for_interpreter = self.new_child();

        if let Some(init) = initializer {
            for_interpreter.eval_ast(init)?;
        }

        loop {
            if let Some(cond) = condition {
                let cond_value = for_interpreter.eval_ast(cond)?;
                if !Self::is_truthy(&cond_value) {
                    break;
                }
            }
            if let Some(stmt) = statement {
                let value = for_interpreter.eval_ast(stmt)?;
                if let Value::Return(_) = &value {
                    return Ok(value);
                }
            }
            if let Some(incr) = increment {
                for_interpreter.eval_ast(incr)?;
            }
        }

        Ok(Value::Nil)
    }

    fn eval_print_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        let value = self.eval_ast(&ast_node.get_children()[1])?;
        println!("{value}");
        Ok(Value::Nil)
    }

    fn eval_return_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children();
        let value = if children.len() == 1 {
            self.eval_ast(&children[0])?
        } else {
            Value::Nil
        };

        Ok(Value::Return(Box::new(value)))
    }

    fn eval_expr_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        self.eval_ast(&ast_node.get_children()[0])?;
        Ok(Value::Nil)
    }

    fn eval_unary(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children();

        let operator_type = match &children[0] {
            Ast::Terminal(token) => &token.token_type,
            _ => return Self::error("expected unary operator"),
        };

        let value = self.eval_ast(&children[1])?;

        match operator_type {
            TokenType::Minus => match value {
                Value::Number(num) => Ok(Value::Number(-num)),
                _ => Self::error("operand must be a number"),
            },
            TokenType::Bang => Ok(Value::Boolean(!Self::is_truthy(&value))),
            _ => Self::error("invalid unary operator"),
        }
    }

    fn eval_binary(&self, ast_node: &AstNode) -> InterpreterResult {
        let operator = match ast_node.get_value() {
            Some(AstValue::Str(op)) => op.as_str(),
            _ => return Self::error("expected binary operator"),
        };

        if operator == "." {
            return self.eval_instance_member(ast_node);
        }

        let children = ast_node.get_children();
        let left = self.eval_ast(&children[0])?;
        let right = self.eval_ast(&children[2])?;

        let (both_nums, both_strings) = match (&left, &right) {
            (Value::Number(_), Value::Number(_)) => (true, false),
            (Value::Str(_), Value::Str(_)) => (false, true),
            _ => (false, false),
        };

        match operator {
            "+" => {
                if !both_nums && !both_strings {
                    return Self::error("operands must be all numbers or all strings");
                }
                if both_nums {
                    let a = match left {
                        Value::Number(num) => num,
                        _ => panic!("must not happen"),
                    };
                    let b = match right {
                        Value::Number(num) => num,
                        _ => panic!("must not happen"),
                    };
                    Ok(Value::Number(a + b))
                } else {
                    let a = match left {
                        Value::Str(s) => s,
                        _ => panic!("must not happen"),
                    };
                    let b = match right {
                        Value::Str(s) => s,
                        _ => panic!("must not happen"),
                    };
                    let mut s = String::new();
                    s.push_str(&a);
                    s.push_str(&b);
                    Ok(Value::Str(s))
                }
            }
            "==" => Ok(Value::Boolean(Self::are_equal(&left, &right))),
            "!=" => Ok(Value::Boolean(!Self::are_equal(&left, &right))),
            _ => {
                if !both_nums {
                    return Self::error("operands must be numbers");
                }

                let a = match left {
                    Value::Number(num) => num,
                    _ => panic!("must not happen"),
                };
                let b = match right {
                    Value::Number(num) => num,
                    _ => panic!("must not happen"),
                };

                match operator {
                    "-" => Ok(Value::Number(a - b)),
                    "*" => Ok(Value::Number(a * b)),
                    "/" => {
                        if b > f64::EPSILON {
                            Ok(Value::Number(a / b))
                        } else {
                            Self::error("cannot divide by zero")
                        }
                    }
                    ">" => Ok(Value::Boolean(a > b)),
                    ">=" => Ok(Value::Boolean(a >= b)),
                    "<" => Ok(Value::Boolean(a < b)),
                    "<=" => Ok(Value::Boolean(a <= b)),
                    _ => Self::error("unsupported binary operator"),
                }
            }
        }
    }

    fn eval_instance_member(&self, path: &AstNode) -> InterpreterResult {
        let children = path.get_children();
        if children.len() != 3 {
            return Self::error("invalid instance member access");
        }

        let instance = self.eval_ast(&children[0])?;
        if let Value::Instance(instance) = instance {
            let (member_node, is_member) = match &children[2] {
                Ast::NonTerminal(ast_node) => match ast_node.get_type() {
                    AstType::VarRef => (ast_node, true),
                    AstType::Call => (ast_node, false),
                    _ => return Self::error("invalid instance member access"),
                },
                _ => return Self::error("invalid instance member access"),
            };

            if is_member {
                let member_name = Self::get_field_name(&children[2])?;
                instance
                    .borrow()
                    .get_member(&member_name)
                    .ok_or(LoxError::new_in_eval_ctx(format!(
                        "Unknown member {member_name}"
                    )))
            } else {
                self.eval_method_call(instance.clone(), member_node)
            }
        } else {
            Self::error("invalid instance")
        }
    }

    fn eval_method_call(
        &self,
        instance: Rc<RefCell<Instance>>,
        method_call_node: &AstNode,
    ) -> InterpreterResult {
        let children = method_call_node.get_children();

        let mut args = vec![];
        for child in children.iter().skip(1) {
            args.push(self.eval_ast(child)?);
        }

        match &children[0] {
            Ast::NonTerminal(ast_node) => match ast_node.get_type() {
                AstType::VarRef => {
                    match ast_node.get_value_str() {
                        Some(name) => instance.borrow().call_method(&name, args),
                        None => Self::error("invalid method call"),
                    }
                }
                AstType::Call => {
                    let func = self.eval_method_call(instance.clone(), ast_node)?;
                    match func {
                        Value::UserFunc(user_func) => user_func.call(args),
                        _ => Self::error("invalid method call"),
                    }
                }
                _ => Self::error("invalid method call"),
            },
            _ => Self::error("invalid method call"),
        }
    }

    fn eval_assignment(&self, assign_node: &AstNode) -> InterpreterResult {
        let children = assign_node.get_children();
        let lhs = &children[0];
        let (lhs, is_lhs_identifier) = match lhs {
            Ast::NonTerminal(ast_node) => match ast_node.get_type() {
                AstType::VarRef => (ast_node, true),
                _ => (ast_node, false),
            },
            _ => return Self::error("invalid lhs of assignment"),
        };

        let rhs_value = self.eval_ast(&children[2])?;

        if is_lhs_identifier {
            let identifier = lhs.get_value_str().ok_or(LoxError::new_in_eval_ctx(
                "invalid variable reference".to_string(),
            ))?;
            let scope_level = match lhs.get_attr("scope_level") {
                Some(AstValue::Int(level)) => *level,
                _ => -1,
            };

            if scope_level != -1 {
                if Env::update_value_at_level(
                    self.env.clone(),
                    identifier.clone(),
                    rhs_value.clone(),
                    scope_level,
                ) {
                    Ok(rhs_value)
                } else {
                    Self::error(&format!("variable {identifier} does not exist"))
                }
            } else if self
                .env
                .borrow_mut()
                .update_value(identifier.clone(), rhs_value.clone())
            {
                Ok(rhs_value)
            } else {
                Self::error(&format!("variable {identifier} does not exist"))
            }
        } else {
            self.set_instance_field(lhs, rhs_value)
        }
    }

    fn set_instance_field(&self, field_path: &AstNode, value: Value) -> InterpreterResult {
        let children = field_path.get_children();
        let instance = self.eval_ast(&children[0])?;
        if let Value::Instance(instance) = instance {
            let field_name = Self::get_field_name(&children[2])?;
            instance.borrow_mut().set_field(field_name, value.clone());
            Ok(value)
        } else {
            Self::error("invalid instance")
        }
    }

    fn get_field_name(ast: &Ast) -> Result<String, LoxError> {
        match ast {
            Ast::Terminal(_) => Err(LoxError::new_in_eval_ctx("invalid field name".to_string())),
            Ast::NonTerminal(ast_node) if ast_node.get_type() == &AstType::VarRef => {
                match ast_node.get_value_str() {
                    Some(name) => Ok(name),
                    None => Err(LoxError::new_in_eval_ctx("invalid field name".to_string())),
                }
            }
            _ => Err(LoxError::new_in_eval_ctx("invalid field name".to_string())),
        }
    }

    fn eval_disjunction(&self, ast_node: &AstNode) -> InterpreterResult {
        let mut ret = Value::Boolean(true);
        let operands = ast_node.get_children();
        for operand in operands {
            ret = self.eval_ast(operand)?;
            if Self::is_truthy(&ret) {
                return Ok(ret);
            }
        }

        Ok(ret)
    }

    fn eval_conjunction(&self, ast_node: &AstNode) -> InterpreterResult {
        let mut ret = Value::Boolean(false);
        let operands = ast_node.get_children();
        for operand in operands {
            ret = self.eval_ast(operand)?;
            if !Self::is_truthy(&ret) {
                return Ok(ret);
            }
        }

        Ok(ret)
    }

    fn eval_call(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children();
        let callee = self.eval_ast(&children[0])?;
        let args = children[1..]
            .iter()
            .map(|arg| self.eval_ast(arg))
            .collect::<Result<Vec<Value>, LoxError>>()?;

        match callee {
            Value::NativeFunc(native_fn) => native_fn.call(args),
            Value::UserFunc(user_fn) => user_fn.call(args),
            Value::ClassDef(class) => class_call(class, args),
            _ => Self::error("invalid callee"),
        }
    }

    fn eval_var_ref(&self, ast_node: &AstNode) -> InterpreterResult {
        let ast_value_opt = ast_node.get_value();
        let identifier = match ast_value_opt {
            Some(AstValue::Str(identifier)) => identifier,
            _ => return Self::error("invalid variable reference"),
        };

        let scope_level = match ast_node.get_attr("scope_level") {
            Some(AstValue::Int(level)) => *level,
            _ => -1,
        };

        if scope_level != -1 {
            match Env::get_value_at_level(self.env.clone(), identifier, scope_level) {
                Some(value) => Ok(value),
                None => Self::error(&format!("identifier {identifier} is unknown")),
            }
        } else {
            match self.env.borrow().get_value(identifier) {
                Some(value) => Ok(value),
                None => Self::error(&format!("identifier {identifier} is unknown")),
            }
        }
    }

    fn are_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => (a - b).abs() < f64::EPSILON,
            (Value::Str(a), Value::Str(b)) => a == b,
            _ => false,
        }
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    fn error(msg: &str) -> Result<Value, LoxError> {
        Err(LoxError::new_in_eval_ctx(msg.to_string()))
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_nil() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("nil".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "nil");
    }

    #[test]
    fn eval_bool() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("true".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "true");
    }

    #[test]
    fn eval_number() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("42".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "42");

        let value = interpreter
            .eval("3.1415".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "3.1415");
    }

    #[test]
    fn eval_str() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("\"Thomas\"".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "Thomas");
    }

    #[test]
    fn eval_group() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("(\"Thomas\")".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "Thomas");
    }

    #[test]
    fn eval_negative() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("-42".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "-42");
    }

    #[test]
    fn eval_not() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("!42".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "false");
    }

    #[test]
    fn eval_add() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("20 + 22".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "42");
    }

    #[test]
    fn eval_subtract() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("20 - 22".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "-2");
    }

    #[test]
    fn eval_multiply() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("7.0 * 6".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "42");
    }

    #[test]
    fn eval_divide() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("84 / 2".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "42");
    }

    #[test]
    fn eval_concat() {
        let interpreter = Interpreter::new();
        let value = interpreter
            .eval("\"foo\" + \"bar\"".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "foobar");
    }

    #[test]
    fn eval_if_stmt() {
        let interpreter = Interpreter::new();
        let code = r#"
            var answer = 42;
            if (answer == 42) {
                print "foo";
            } else {
                print "bar";
            }
        "#;
        assert!(interpreter.run(code.to_string()).is_ok());
    }

    #[test]
    fn eval_for_stmt() {
        let interpreter = Interpreter::new();
        let code = r#"
            for (var baz = 0; baz < 3;) print baz = baz + 1;
        "#;
        assert!(interpreter.run(code.to_string()).is_ok());
    }

    #[test]
    fn eval_return_stmt() {
        let interpreter = Interpreter::new();
        let code = r#"
            fun add(a, b) {
                return a + b;
            }
            print add(20, 22);
        "#;
        let result = interpreter.run(code.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap().get_message());
    }

    #[test]
    fn eval_counter() {
        let interpreter = Interpreter::new();
        let code = r#"
            var counter = 0;
            {
                fun increment() {
                    counter = counter + 1;
                    return counter;
                }
                print increment();

                var counter = 0;

                print increment();
            }
        "#;
        let result = interpreter.run(code.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap().get_message());
    }

    #[test]
    fn eval_set_field() {
        let interpreter = Interpreter::new();
        let code = r#"
            class Foo {}
            var foo = Foo();
            foo.bar = 42;

            print foo.bar;
        "#;
        let result = interpreter.run(code.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap().get_message());
    }

    #[test]
    fn eval_method_call() {
        let interpreter = Interpreter::new();
        let code = r#"
            class Robot {
                beep() {
                    print "beep";
                }
            }
            var robot = Robot();
            robot.beep();
        "#;
        let result = interpreter.run(code.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap().get_message());
    }

    #[test]
    fn eval_this() {
        let interpreter = Interpreter::new();
        let code = r#"
            class Animal {
                makeSound() {
                    print this.sound;
                }
                identify() {
                    print "I am a " + this.type;
                }
            }
            var dog = Animal();
            dog.sound = "bark";
            dog.type = "dog";

            var cat = Animal();
            cat.sound = "meow";
            cat.type = "cat";

            cat.makeSound = dog.makeSound;
            dog.identify = cat.identify;

            cat.makeSound();
            dog.identify();
        "#;
        let result = interpreter.run(code.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap().get_message());
    }

    #[test]
    fn eval_higher_order() {
        let interpreter = Interpreter::new();
        let code = r#"
            class GreeterFactory {
                makeGreeter() {
                    fun greeter() {
                        print "Hello, " + this.name + "!";
                    }
                    return greeter;
                }
            }

            var greeterFactory = GreeterFactory();
            greeterFactory.name = "Thomas";

            greeterFactory.makeGreeter()();
        "#;
        let result = interpreter.run(code.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap().get_message());
    }

}
