use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum ErrorContext {
    Lexical,
    Parser,
    Interpreter,
}

#[derive(Debug, Clone)]
pub struct LoxError {
    context: ErrorContext,
    message: String,
    line: Option<usize>,
}

impl LoxError {
    fn new(context: ErrorContext, message: String, line: Option<usize>) -> LoxError {
        LoxError {
            context,
            message,
            line,
        }
    }
    
    pub fn new_in_lexical_ctx(message: String, line: usize) -> LoxError {
        LoxError::new(ErrorContext::Lexical, message, Some(line))
    }

    pub fn new_in_parser_ctx(message: String) -> LoxError {
        LoxError::new(ErrorContext::Parser, message, None)
    }

    pub fn new_in_eval_ctx(message: String) -> LoxError {
        LoxError::new(ErrorContext::Interpreter, message, None)
    }
    
    pub fn get_context(&self) -> &ErrorContext {
        &self.context
    }

    pub fn get_line(&self) -> usize {
        self.line.unwrap_or(0)
    }

    pub fn get_message(&self) -> &str {
        &self.message
    }
}

impl Display for LoxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LoxError {}
