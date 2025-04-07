use crate::frontend::ast::Ast::{NonTerminal, Terminal};
use crate::frontend::ast::{Ast, AstNode, AstType, AstValue};
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::Stream;
use crate::frontend::tokens::{Token, TokenType};
use std::error::Error;
use std::fmt::{Display, Formatter};

pub struct Parser {
    scanner: Scanner,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        Parser { scanner }
    }

    pub fn expression(&mut self) -> Result<Ast, Box<dyn Error>> {
        let token = self.advance()?.ok_or(Box::new(ParseError::new(
            "expected token but got none".to_string(),
        )))?;

        match token.token_type {
            TokenType::True | TokenType::False => {
                let value = token.lexeme == "true";
                let mut ast_node = AstNode::new(AstType::Boolean, Some(AstValue::Boolean(value)));
                ast_node.add_child(Terminal(token));
                Ok(NonTerminal(ast_node))
            }
            TokenType::Nil => {
                let mut ast_node = AstNode::new(AstType::Nil, None);
                ast_node.add_child(Terminal(token));
                Ok(NonTerminal(ast_node))
            }
            _ => Err(Box::new(ParseError::new("unexpected token".to_string()))),
        }
    }

    fn advance(&mut self) -> Result<Option<Token>, Box<dyn Error>> {
        self.scanner.next().map_err(|e| {
            let err: Box<dyn Error> = Box::new(e);
            err
        })
    }
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl ParseError {
    fn new(message: String) -> ParseError {
        ParseError { message }
    }

    pub fn get_message(&self) -> &str {
        &self.message
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ParseError {}
