use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstVisitor};
use crate::frontend::parser::Parser;
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::CharStream;
use crate::frontend::tokens::{Literal, TokenType};
use crate::interpreter::values::Value;

pub mod values;

pub struct Interpreter {
    last_result: Result<Value, LoxError>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            last_result: Ok(Value::Nil),
        }
    }

    pub fn eval(&mut self, code: String) -> Result<Value, LoxError> {
        let scanner = Scanner::new(CharStream::new(code));
        let mut parser = Parser::new(scanner);
        let ast = parser.expression()?;

        self.eval_ast(&ast)
    }

    fn eval_ast(&mut self, ast: &Ast) -> Result<Value, LoxError> {
        ast.accept(self);
        match &self.last_result {
            Ok(value) => Ok(value.clone()),
            Err(err) => Err(err.clone()),
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

impl AstVisitor for Interpreter {
    fn visit(&mut self, ast: &Ast) {
        match ast {
            Ast::Terminal(token) => {
                self.last_result = match token.token_type {
                    TokenType::Nil => Ok(Value::Nil),
                    TokenType::True => Ok(Value::Boolean(true)),
                    TokenType::False => Ok(Value::Boolean(false)),
                    TokenType::Number => {
                        match token.literal {
                            Literal::Number(num) => Ok(Value::Number(num)),
                            _ => Self::error("invalid literal value")
                        }    
                    }
                    TokenType::Str => {
                        match &token.literal {
                            Literal::Str(s) => Ok(Value::Str(s.clone())),
                            _ => Self::error("invalid literal value")
                        }
                    }
                    _ => Self::error("unsupported token"),
                };
            }
            Ast::NonTerminal(_) => {
                self.last_result = Self::error("not implemented");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn eval_nil() {
        let mut interpreter = Interpreter::new();
        let value = interpreter
            .eval("nil".to_string())
            .expect("error in evaluation");
        
        assert_eq!(&format!("{}", value), "nil");
    }

    #[test]
    fn eval_bool() {
        let mut interpreter = Interpreter::new();
        let value = interpreter
            .eval("true".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "true");
    }

    #[test]
    fn eval_number() {
        let mut interpreter = Interpreter::new();
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
        let mut interpreter = Interpreter::new();
        let value = interpreter
            .eval("\"Thomas\"".to_string())
            .expect("error in evaluation");

        assert_eq!(&format!("{}", value), "Thomas");
    }
    
}
