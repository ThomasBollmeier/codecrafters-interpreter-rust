use crate::frontend::ast::{Ast, AstType, AstValue, AstVisitorMut};
use crate::frontend::tokens::TokenType;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

pub struct VarResolver {
    scope: Rc<RefCell<Scope>>,
}

impl VarResolver {
    pub fn new() -> VarResolver {
        VarResolver {
            scope: Rc::new(RefCell::new(Scope::new(None))),
        }
    }

    fn enter_scope(&mut self) {
        self.scope = Rc::new(RefCell::new(Scope::new_child(self.scope.clone())));
    }

    fn exit_scope(&mut self) {
        let new_scope = self.scope.borrow().parent().unwrap();
        self.scope = new_scope;
    }
}

impl AstVisitorMut for VarResolver {
    fn visit(&mut self, ast: &mut Ast) {
        match ast {
            Ast::NonTerminal(node) => {
                match node.get_type() {
                    AstType::Block | AstType::ForStmt => {
                        self.enter_scope();
                    }
                    AstType::FunDecl => {
                        let fun_name = node.get_value();
                        if let Some(name) = fun_name {
                            if let AstValue::Str(s) = name {
                                self.scope.borrow_mut().add_variable(s.clone());
                            } else {
                                panic!("Function name should be a string");
                            }
                        } else {
                            panic!("Function name should be present");
                        }
                        self.enter_scope();
                        self.enter_scope();

                        for child in node.get_children() {
                            if let Ast::Terminal(token) = child {
                                if token.token_type == TokenType::Identifier {
                                    self.scope.borrow_mut().add_variable(token.lexeme.clone());
                                }
                            }
                        }
                    }
                    AstType::VarDecl => {
                        let var_name = node.get_value();
                        if let Some(name) = var_name {
                            if let AstValue::Str(s) = name {
                                self.scope.borrow_mut().add_variable(s.clone());
                            } else {
                                panic!("Variable name should be a string");
                            }
                        } else {
                            panic!("Variable name should be present");
                        }
                    }
                    AstType::VarRef => {
                        let var_name = node.get_value();
                        if let Some(name) = var_name {
                            if let AstValue::Str(s) = name {
                                if let Some(level) =
                                    self.scope.borrow().get_var_scope_level(s.clone())
                                {
                                    node.set_attr_int("scope_level".to_string(), level as i32);
                                }
                            } else {
                                panic!("Variable name should be a string");
                            }
                        } else {
                            panic!("Variable name should be present");
                        }
                    }
                    _ => {}
                }
                for child in node.get_children_mut() {
                    child.accept_mut(self);
                }
                match node.get_type() {
                    AstType::Block | AstType::ForStmt => {
                        self.exit_scope();
                    }
                    AstType::FunDecl => {
                        self.exit_scope();
                        self.exit_scope();
                    }
                    _ => {}
                }
            }
            Ast::Terminal(_) => {}
        }
    }
}

struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    variables: HashSet<String>,
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>) -> Self {
        Scope {
            parent,
            variables: HashSet::new(),
        }
    }

    fn parent(&self) -> Option<Rc<RefCell<Scope>>> {
        self.parent.clone()
    }

    fn new_child(scope: Rc<RefCell<Scope>>) -> Self {
        Scope::new(Some(scope))
    }

    fn add_variable(&mut self, name: String) {
        self.variables.insert(name);
    }

    fn get_var_scope_level(&self, name: String) -> Option<u32> {
        if self.variables.contains(&name) {
            return Some(0);
        }
        match &self.parent {
            Some(parent) => {
                let parent_scope = parent.borrow();
                parent_scope
                    .get_var_scope_level(name)
                    .map(|parent_level| 1 + parent_level)
            }
            None => None,
        }
    }
}
