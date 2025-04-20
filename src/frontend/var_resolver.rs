use crate::common::LoxError;
use crate::frontend::ast::{Ast, AstType, AstVisitorMut};
use crate::frontend::tokens::TokenType;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

pub struct VarResolver {
    scope: Rc<RefCell<Scope>>,
    error: Option<LoxError>,
    var_name_in_decl: Option<String>,
    fun_nesting_level: i32,
}

impl VarResolver {
    pub fn new() -> VarResolver {
        VarResolver {
            scope: Rc::new(RefCell::new(Scope::new(None, false))),
            error: None,
            var_name_in_decl: None,
            fun_nesting_level: 0,
        }
    }

    pub fn resolve(&mut self, ast: &mut Ast) -> Option<LoxError> {
        self.visit(ast);
        self.error.clone()
    }

    fn enter_scope(&mut self, is_param_scope: bool) {
        self.scope = Rc::new(RefCell::new(Scope::new_child(self.scope.clone(), is_param_scope)));
    }

    fn exit_scope(&mut self) {
        let new_scope = self.scope.borrow().parent().unwrap();
        self.scope = new_scope;
    }

    fn set_error(&mut self, msg: &str) {
        self.error = Some(LoxError::new_in_parser_ctx(msg.to_string()));
    }
}

impl AstVisitorMut for VarResolver {
    fn visit(&mut self, ast: &mut Ast) {
        match ast {
            Ast::NonTerminal(node) => {
                match node.get_type() {
                    AstType::Block | AstType::ForStmt => {
                        self.enter_scope(false);
                    }
                    AstType::FunDecl => {
                        if let Some(fun_name) = node.get_value_str() {
                            self.scope.borrow_mut().add_variable(fun_name);
                        } else {
                            self.set_error("Function name should be present");
                            return;
                        }
                        self.enter_scope(false);
                        self.enter_scope(true);
                        self.fun_nesting_level += 1;

                        for child in node.get_children() {
                            if let Ast::Terminal(token) = child {
                                if token.token_type == TokenType::Identifier {
                                    let var_name = token.lexeme.clone();
                                    if self.scope.borrow().has_variable(&var_name) {
                                        self.set_error("Variable already declared in this scope");
                                        return;
                                    }
                                    self.scope.borrow_mut().add_variable(var_name);
                                }
                            }
                        }
                    }
                    AstType::VarDecl => {
                        if let Some(var_name) = node.get_value_str() {
                            let already_declared = {
                                let scope = self.scope.borrow();
                                scope.has_variable(&var_name) && !scope.is_global()
                            };
                            if already_declared {
                                self.set_error("Variable already declared in this scope");
                                return;
                            } 
                            self.var_name_in_decl = Some(var_name.clone());
                            self.scope.borrow_mut().add_variable(var_name);
                        } else {
                            self.set_error("Variable name should be present");
                        }
                    }
                    AstType::VarRef => {
                        if let Some(name) = node.get_value_str() {
                            match self.var_name_in_decl {
                                Some(ref var_name)
                                    if name == *var_name && !self.scope.borrow().is_global() =>
                                {
                                    self.set_error("Variable cannot be used before declaration");
                                }
                                _ => {}
                            }
                            if self.error.is_none() {
                                if let Some(level) = self.scope.borrow().get_var_scope_level(name) {
                                    node.set_attr_int("scope_level".to_string(), level as i32);
                                }
                            }
                        } else {
                            self.set_error("Variable name should be present");
                        }
                    }
                    AstType::ReturnStmt => {
                        if self.fun_nesting_level == 0 {
                            self.set_error("Return statement outside of function");
                            return;
                        }
                    }
                    _ => {}
                }

                if self.error.is_some() {
                    return;
                }

                for child in node.get_children_mut() {
                    child.accept_mut(self);
                    if self.error.is_some() {
                        return;
                    }
                }

                match node.get_type() {
                    AstType::Block | AstType::ForStmt => {
                        self.exit_scope();
                    }
                    AstType::FunDecl => {
                        self.exit_scope();
                        self.exit_scope();
                        self.fun_nesting_level -= 1;
                    }
                    AstType::VarDecl => self.var_name_in_decl = None,
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
    is_param_scope: bool,
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>, is_param_scope: bool) -> Self {
        Scope {
            parent,
            variables: HashSet::new(),
            is_param_scope
        }
    }

    fn parent(&self) -> Option<Rc<RefCell<Scope>>> {
        self.parent.clone()
    }

    fn is_global(&self) -> bool {
        self.parent.is_none()
    }

    fn new_child(scope: Rc<RefCell<Scope>>, is_param_scope: bool) -> Self {
        Scope::new(Some(scope), is_param_scope)
    }

    fn add_variable(&mut self, name: String) {
        self.variables.insert(name);
    }

    fn has_variable(&self, name: &str) -> bool {
        if self.variables.contains(name) {
            return true;
        }
        match &self.parent {
            Some(parent) => {
                let p = parent.borrow();
                p.is_param_scope && p.variables.contains(name)
            },
            None => false,
        }
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
