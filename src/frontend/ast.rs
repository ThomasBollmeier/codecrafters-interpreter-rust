use crate::frontend::tokens::Token;
use std::collections::HashMap;

pub enum AstType {
    Program,
    VarDecl,
    Block,
    IfStmt,
    WhileStmt,
    ForStmt,
    PrintStmt,
    ExprStmt,
    Group,
    Unary,
    Binary,
    Assignment,
    Disjunction,
    Conjunction,
    Call,
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

    pub fn set_attr_bool(&mut self, key: String, value: bool) {
        self.attrs.insert(key, AstValue::Boolean(value));
    }

    pub fn get_attr(&self, key: &str) -> Option<&AstValue> {
        self.attrs.get(key)
    }

    pub fn has_attr(&self, key: &str) -> bool {
        self.attrs.contains_key(key)
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

    pub fn set_label(&mut self, label: &str) {
        if let Ast::NonTerminal(ast_node) = self {
            ast_node.set_attr_str("label".to_string(), label);
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            Ast::NonTerminal(ast_node) => match ast_node.get_attr("label") {
                Some(value) => match value {
                    AstValue::Str(s) => s.clone(),
                    _ => String::new(),
                },
                _ => String::new(),
            },
            _ => String::new(),
        }
    }
}

pub trait AstVisitor {
    fn visit(&mut self, ast: &Ast);
}

pub trait AstVisitorMut {
    fn visit(&mut self, ast: &mut Ast);
}
