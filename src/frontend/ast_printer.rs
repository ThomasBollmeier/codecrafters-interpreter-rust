use crate::frontend::ast::{Ast, AstNode, AstType, AstVisitor};
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
    
    fn str(&self, ast: &Ast) -> String {
        match ast {
            Ast::NonTerminal(ast_node) => {
                match ast_node.get_type() {
                    AstType::Group => self.str_group(ast_node),
                }
            }
            Ast::Terminal(token) => {
                match token.token_type {
                    TokenType::True | TokenType::False => self.str_boolean(token),
                    TokenType::Nil => self.str_nil(),
                    TokenType::Number => self.str_number(token),
                    TokenType::Str => self.str_string(token),
                    _ => { "".to_string() }
                }
            }
        }
    }

    fn str_boolean(&self, token: &Token) -> String {
        token.lexeme.clone()
    }
    
    fn str_nil(&self) -> String {
        "nil".to_string()
    }
    
    fn str_number(&self, token: &Token) -> String {
        if let Literal::Number(num) = token.literal {
            format!("{num:?}")
        } else {
            String::new()
        }
    }
    
    fn str_string(&self, token: &Token) -> String {
        if let Literal::Str(s) = &token.literal {
            s.clone()
        } else {
            String::new()
        }
    }
    
    fn str_group(&self, group: &AstNode) -> String {
        let mut ret = String::new();
        ret.push_str("(group ");
        let expr = &group.get_children()[1];
        ret.push_str(&self.str(expr));
        ret.push(')');
        ret
    }
}

impl AstVisitor for AstPrinter {
    fn visit(&self, ast: &Ast) {
        let s = self.str(ast);
        println!("{s}");
    }
}