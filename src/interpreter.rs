use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstNode, AstType};
use crate::frontend::parser::Parser;
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::CharStream;
use crate::frontend::tokens::{Literal, TokenType};
use crate::interpreter::values::Value;

pub mod values;

pub type InterpreterResult = Result<Value, LoxError>;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn eval(&self, code: String) -> InterpreterResult {
        let scanner = Scanner::new(CharStream::new(code));
        let mut parser = Parser::new(scanner);
        let ast = parser.expression()?;

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
                _ => Self::error("unsupported token"),
            },
            Ast::NonTerminal(ast_node) => match ast_node.get_type() {
                AstType::Group => {
                    let expr = &ast_node.get_children()[1];
                    self.eval_ast(expr)
                }
                AstType::Unary => self.eval_unary(ast_node),
                _ => Self::error("not implemented"),
            },
        }
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
}
