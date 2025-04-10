use std::collections::VecDeque;
use std::vec;

use crate::common::LoxError;
use crate::frontend::ast::Ast::{NonTerminal, Terminal};
use crate::frontend::ast::{Ast, AstNode, AstType};
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::{BufferedStream, CharStream};
use crate::frontend::tokens::{Token, TokenType};

use super::ast::AstValue;

pub struct Parser {
    token_stream: BufferedStream<Token, LoxError>,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        Parser {
            token_stream: BufferedStream::new(Box::new(scanner)),
        }
    }

    pub fn expression(&mut self) -> Result<Ast, LoxError> {
        let token = self.advance()?.ok_or(LoxError::new_in_parser_ctx(
            "expected token but got none".to_string(),
        ))?;

        self.sum(token)
    }

    fn sum(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut operands = VecDeque::new();
        let mut operators = VecDeque::new();

        let mut next_token = token;

        loop {
            let operand = self.product(next_token)?;
            operands.push_back(operand);

            match self.peek() {
                Some(token) => match token.token_type {
                    TokenType::Plus | TokenType::Minus => {}
                    _ => break,
                },
                None => break,
            }

            let operator = self.advance()?.unwrap();
            operators.push_back(operator);

            next_token = self.advance()?.ok_or(LoxError::new_in_parser_ctx(
                "expected operand but got none".to_string(),
            ))?;
        }

        Ok(Parser::left_assoc_bin_ast(&mut operands, &mut operators))
    }

    fn product(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut operands = VecDeque::new();
        let mut operators = VecDeque::new();

        let mut next_token = token;

        loop {
            let operand = self.atom(next_token)?;
            operands.push_back(operand);

            match self.peek() {
                Some(token) => match token.token_type {
                    TokenType::Star | TokenType::Slash => {}
                    _ => break,
                },
                None => break,
            }

            let operator = self.advance()?.unwrap();
            operators.push_back(operator);

            next_token = self.advance()?.ok_or(LoxError::new_in_parser_ctx(
                "expected operand but got none".to_string(),
            ))?;
        }
        
        Ok(Parser::left_assoc_bin_ast(&mut operands, &mut operators))
    }
    
    fn left_assoc_bin_ast(operands: &mut VecDeque<Ast>, operators: &mut VecDeque<Token>) -> Ast {
        if operands.len() == 1 {
            return operands.pop_front().unwrap();
        }

        let mut operator = operators.pop_front().unwrap();
        let mut ret = AstNode::new(
            AstType::Binary,
            Some(AstValue::Str(operator.lexeme.clone())),
        );
        ret.add_child(operands.pop_front().unwrap());
        ret.add_child(Terminal(operator));
        ret.add_child(operands.pop_front().unwrap());

        while !operators.is_empty() {
            operator = operators.pop_front().unwrap();
            let mut binary_node = AstNode::new(
                AstType::Binary,
                Some(AstValue::Str(operator.lexeme.clone())),
            );
            binary_node.add_child(NonTerminal(ret));
            binary_node.add_child(Terminal(operator));
            binary_node.add_child(operands.pop_front().unwrap());
            ret = binary_node;
        }
        
        NonTerminal(ret)
    }

    fn atom(&mut self, token: Token) -> Result<Ast, LoxError> {
        match token.token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::Number
            | TokenType::Str => Ok(Terminal(token)),
            TokenType::LeftParen => self.group(token),
            TokenType::Bang | TokenType::Minus => self.unary(token),
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

    fn unary(&mut self, operator: Token) -> Result<Ast, LoxError> {
        let mut unary_node = AstNode::new(AstType::Unary, None);
        unary_node.add_child(Terminal(operator));
        let next_token = self.advance()?.ok_or(
            LoxError::new_in_parser_ctx("expected token but got none".to_string())
        )?;
        unary_node.add_child(self.atom(next_token)?);

        Ok(NonTerminal(unary_node))
    }

    fn peek(&mut self) -> Option<&Token> {
        self.token_stream.peek()
    }

    fn advance(&mut self) -> Result<Option<Token>, LoxError> {
        self.token_stream.advance()
    }

    fn consume(&mut self, expected: &Vec<TokenType>) -> Result<Token, LoxError> {
        let next_token = self.token_stream.peek().ok_or(LoxError::new_in_parser_ctx(
            "expected token but got none".to_string(),
        ))?;

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
            Err(LoxError::new_in_parser_ctx(
                "token has unexpected type".to_string(),
            ))
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
    use crate::frontend::ast_printer::AstPrinter;

    #[test]
    fn group() {
        let result = parse_expression("(42)");
        assert!(result.is_ok());

        let ast_printer = AstPrinter::new();
        assert_eq!("(group 42.0)", ast_printer.str(&result.unwrap()))
    }

    #[test]
    fn unary() {
        let result = parse_expression("!true");
        assert!(result.is_ok());

        let ast_printer = AstPrinter::new();
        assert_eq!("(! true)", ast_printer.str(&result.unwrap()))
    }

    #[test]
    fn binary() {
        let result = parse_expression("(39 * -22 / (43 * 54))");
        assert!(result.is_ok());

        let ast_printer = AstPrinter::new();
        assert_eq!(
            "(group (/ (* 39.0 (- 22.0)) (group (* 43.0 54.0))))",
            ast_printer.str(&result.unwrap())
        )
    }
}
