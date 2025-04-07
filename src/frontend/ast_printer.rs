use crate::frontend::ast::{Ast, AstNode, AstType, AstValue, AstVisitor};

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

    fn print_boolean(&self, boolean: &AstNode) {
        if let Some(AstValue::Boolean(value)) = boolean.get_value() {
            println!("{}", if *value {
                "true"
            } else {
                "false"
            });
        }
    }
    
    fn print_nil(&self) {
        println!("nil");
    }
}

impl AstVisitor for AstPrinter {
    fn visit(&self, ast: &Ast) {
        match ast {
            Ast::NonTerminal(ast_node) => {
                match ast_node.get_type() {
                    AstType::Boolean => self.print_boolean(ast_node),
                    AstType::Nil => self.print_nil(),
                }
            }
            Ast::Terminal(_) => {} 
        }
    }
    
}