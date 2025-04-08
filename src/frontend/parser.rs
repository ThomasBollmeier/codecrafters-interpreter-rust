use crate::common::LoxError;
use crate::frontend::ast::Ast;
use crate::frontend::ast::Ast::Terminal;
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::Stream;
use crate::frontend::tokens::{Token, TokenType};

pub struct Parser {
    scanner: Scanner,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        Parser { scanner }
    }

    pub fn expression(&mut self) -> Result<Ast, LoxError> {
        let token = self.advance()?.ok_or(LoxError::new_in_parser_ctx(
            "expected token but got none".to_string(),
        ))?;

        match token.token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::Number
            | TokenType::Str => Ok(Terminal(token)),
            _ => Err(LoxError::new_in_parser_ctx("unexpected token".to_string())),
        }
    }

    fn advance(&mut self) -> Result<Option<Token>, LoxError> {
        self.scanner.next()
    }
}
