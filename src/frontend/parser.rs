use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstNode, AstType};
use crate::frontend::ast::Ast::{NonTerminal, Terminal};
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::{BufferedStream, CharStream};
use crate::frontend::tokens::{Token, TokenType};

pub struct Parser {
    token_stream: BufferedStream<Token, LoxError>,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        Parser { token_stream: BufferedStream::new(Box::new(scanner)) }
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
            TokenType::LeftParen => self.group(token),
            _ => Err(LoxError::new_in_parser_ctx("unexpected token".to_string())),
        }
    }

    fn group(&mut self, open_paren: Token) -> Result<Ast, LoxError> {
        let mut group_node = AstNode::new(AstType::Group, None);
        group_node.add_child(Terminal(open_paren));
        group_node.add_child(self.expression()?);
        group_node.add_child(Terminal(self.consume(&vec![TokenType::RightParen])?));
        
        Ok(NonTerminal(group_node))
    }

    fn advance(&mut self) -> Result<Option<Token>, LoxError> {
        self.token_stream.advance()
    }

    fn consume(&mut self, expected: &Vec<TokenType>) -> Result<Token, LoxError> {
        let next_token = self.token_stream
            .peek()
            .ok_or(LoxError::new_in_parser_ctx("expected token, but got none".to_string()))?;
        
        let mut found = false;
        for exp in expected {
            if &next_token.token_type == exp {
                found = true;
                break;
            }
        }
        
        if found {
            let token = self.token_stream.advance()?.unwrap();
            Ok(token)
        } else {
            Err(LoxError::new_in_parser_ctx("token has unexpected type".to_string()))
        }
    }
}

pub fn parse_expression(code: &str) -> Result<Ast, LoxError> {
    let scanner = Scanner::new(CharStream::new(code.to_string()));
    let mut parser = Parser::new(scanner);
    parser.expression()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn group() {
        let result = parse_expression("(42)");
        assert!(result.is_ok());
    }
    
}