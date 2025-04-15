use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstNode, AstType, AstValue};
use crate::frontend::parser;
use crate::frontend::tokens::{Literal, TokenType};
use crate::interpreter::env::{Env, EnvRef};
use crate::interpreter::values::Value;

mod env;
pub mod values;

pub type InterpreterResult = Result<Value, LoxError>;

pub struct Interpreter {
    env: EnvRef,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Env::new_ref(None),
        }
    }

    fn new_child(&self) -> Interpreter {
        Interpreter {
            env: Env::new_ref(Some(self.env.clone()))
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
                TokenType::Identifier => self.eval_identifier(&token.lexeme),
                _ => Self::error("unsupported token"),
            },
            Ast::NonTerminal(ast_node) => match ast_node.get_type() {
                AstType::Program => self.eval_program(ast_node),
                AstType::VarDecl => self.eval_var_decl(ast_node),
                AstType::Block => self.eval_block(ast_node),
                AstType::PrintStmt => self.eval_print_stmt(ast_node),
                AstType::ExprStmt => self.eval_expr_stmt(ast_node),
                AstType::Group => {
                    let expr = &ast_node.get_children()[1];
                    self.eval_ast(expr)
                }
                AstType::Unary => self.eval_unary(ast_node),
                AstType::Binary => self.eval_binary(ast_node),
                AstType::Assignment => self.eval_assignment(ast_node),
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

    fn eval_block(&self, ast_node: &AstNode) -> InterpreterResult {
        let children = ast_node.get_children();
        let stmts = &children[1..children.len()-1]; // first + last child are braces

        let child_interpreter = self.new_child();
        for stmt in stmts {
            child_interpreter.eval_ast(stmt)?;
        }

        Ok(Value::Nil)
    }

    fn eval_print_stmt(&self, ast_node: &AstNode) -> InterpreterResult {
        let value = self.eval_ast(&ast_node.get_children()[1])?;
        println!("{value}");
        Ok(Value::Nil)
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

    fn eval_assignment(&self, assign_node: &AstNode) -> InterpreterResult {
        let children = assign_node.get_children();
        let lhs = &children[0];
        let identifier = match lhs {
            Ast::Terminal(token) => token.lexeme.clone(),
            _ => return Self::error("invalid lhs of assignment"),
        };
        let rhs_value = self.eval_ast(&children[2])?;
        if self.env.borrow_mut().update_value(identifier.clone(), rhs_value.clone()) {
            Ok(rhs_value)    
        } else {
            Self::error(&format!("variable {identifier} does not exist"))
        }
    }

    fn eval_identifier(&self, identifier: &str) -> InterpreterResult {
        match self.env.borrow().get_value(identifier) {
            Some(value) => Ok(value),
            None => Self::error(&format!("identifier {identifier} is unknown")),
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
}
