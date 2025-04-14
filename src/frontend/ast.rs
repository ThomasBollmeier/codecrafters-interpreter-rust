use crate::frontend::tokens::Token;
use std::collections::HashMap;

pub enum AstType {
    Program,
    VarDecl,
    PrintStmt,
    ExprStmt,
    Group,
    Unary,
    Binary,
}

pub enum AstValue {
    Str(String),
    Int(i32),
    Number(f64),
    Boolean(bool),
}

pub struct AstNode {
    ast_type: AstType,
    value: Option<AstValue>,
    attrs: HashMap<String, AstValue>,
    children: Vec<Ast>,
}

impl AstNode {
    pub fn new(ast_type: AstType, value: Option<AstValue>) -> AstNode {
        AstNode {
            ast_type,
            value,
            attrs: HashMap::new(),
            children: Vec::new(),
        }
    }

    pub fn get_type(&self) -> &AstType {
        &self.ast_type
    }

    pub fn get_value(&self) -> &Option<AstValue> {
        &self.value
    }

    pub fn set_attr_str(&mut self, key: String, value: &str) {
        self.attrs.insert(key, AstValue::Str(value.to_string()));
    }

    pub fn set_attr_int(&mut self, key: String, value: i32) {
        self.attrs.insert(key, AstValue::Int(value));
    }

    pub fn set_attr_number(&mut self, key: String, value: f64) {
        self.attrs.insert(key, AstValue::Number(value));
    }

    pub fn get_attr(&self, key: &str) -> Option<&AstValue> {
        self.attrs.get(key)
    }

    pub fn add_child(&mut self, child: Ast) {
        self.children.push(child);
    }

    pub fn get_children(&self) -> &Vec<Ast> {
        &self.children
    }
}

pub enum Ast {
    NonTerminal(AstNode),
    Terminal(Token),
}

impl Ast {
    pub fn accept(&self, visitor: &mut impl AstVisitor) {
        visitor.visit(self);
    }

    pub fn accept_mut(&mut self, visitor: &mut impl AstVisitorMut) {
        visitor.visit(self);
    }
}

pub trait AstVisitor {
    fn visit(&mut self, ast: &Ast);
}

pub trait AstVisitorMut {
    fn visit(&mut self, ast: &mut Ast);
}
