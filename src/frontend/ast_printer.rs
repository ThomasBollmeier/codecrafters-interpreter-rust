use crate::frontend::ast::{Ast, AstVisitor};
use crate::frontend::tokens::{Literal, Token, TokenType};

pub struct AstPrinter {}

impl Default for AstPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter {}
    }

    fn print_boolean(&self, token: &Token) {
        println!("{}", token.lexeme);
    }
    
    fn print_nil(&self) {
        println!("nil");
    }
    
    fn print_number(&self, token: &Token) {
        if let Literal::Number(num) = token.literal {
            println!("{num:?}")
        }
    }
}

impl AstVisitor for AstPrinter {
    fn visit(&self, ast: &Ast) {
        match ast {
            Ast::NonTerminal(_ast_node) => {
            }
            Ast::Terminal(token) => {
                match token.token_type {
                    TokenType::True | TokenType::False => self.print_boolean(token),
                    TokenType::Nil => self.print_nil(),
                    TokenType::Number => self.print_number(token),
                    _ => {}
                }
            } 
        }
    }
    
}