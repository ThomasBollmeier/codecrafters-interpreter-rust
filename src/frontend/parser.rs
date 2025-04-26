use crate::common::LoxError;
use crate::frontend::ast::Ast::{NonTerminal, Terminal};
use crate::frontend::ast::{Ast, AstNode, AstType};
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::{BufferedStream, CharStream};
use crate::frontend::tokens::{Token, TokenType};
use std::collections::VecDeque;
use std::vec;
use crate::frontend::var_resolver::VarResolver;
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

    pub fn program(&mut self) -> Result<Ast, LoxError> {
        let mut program_node = AstNode::new(AstType::Program, None);

        while let Some(token) = self.advance()? {
            let stmt = self.declaration(token)?;
            program_node.add_child(stmt);
        }

        if let Some(token) = self.peek() {
            return Err(Self::error(&format!(
                "input is not empty: '{}'",
                token.lexeme
            )));
        }

        let mut resolver = VarResolver::new();
        let mut ast = NonTerminal(program_node);
        if let Some(err) = resolver.resolve(&mut ast) {
            return Err(err);
        }

        Ok(ast)
    }

    fn declaration(&mut self, token: Token) -> Result<Ast, LoxError> {
        match token.token_type {
            TokenType::Var => self.var_decl(token),
            TokenType::Fun => self.fun_decl(),
            TokenType::Class => self.class_decl(),
            _ => self.statement(token),
        }
    }

    fn class_decl(&mut self) -> Result<Ast, LoxError> {
        let class_name = self.consume(&vec![TokenType::Identifier])?;
        let mut class_node = AstNode::new(
            AstType::ClassDecl,
            Some(AstValue::Str(class_name.lexeme.clone())),
        );
        self.consume(&vec![TokenType::LeftBrace])?;

        loop {
            let next_token = match self.peek() {
                Some(token) => token,
                None => return Err(Self::error("expected token, but got none")),
            };
            if next_token.token_type == TokenType::RightBrace {
                self.advance()?;
                break;
            }
            let method = self.fun_decl()?;
            class_node.add_child(method);
        }

        Ok(NonTerminal(class_node))
    }

    fn fun_decl(&mut self) -> Result<Ast, LoxError> {
        let tok_name = self.consume(&vec![TokenType::Identifier])?;
        let mut fun_node = AstNode::new(
            AstType::FunDecl,
            Some(AstValue::Str(tok_name.lexeme.clone())),
        );
        self.consume(&vec![TokenType::LeftParen])?;

        // parse parameters
        loop {
            let next_token = self
                .advance()?
                .ok_or(Self::error("expected token, but got none"))?;
            match next_token.token_type {
                TokenType::RightParen => break,
                TokenType::Identifier => {
                    fun_node.add_child(Terminal(next_token));
                    match self.peek() {
                        Some(token) => match token.token_type {
                            TokenType::Comma => {
                                self.advance()?;
                            }
                            TokenType::RightParen => {}
                            _ => return Err(Self::error("unexpected token type")),
                        },
                        _ => return Err(Self::error("expected token, but got none")),
                    }
                }
                _ => return Err(Self::error("unexpected token type")),
            }
        }

        let next_token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;
        if next_token.token_type != TokenType::LeftBrace {
            return Err(Self::error("expected '{' after parameters"));
        }

        fun_node.add_child(self.block(next_token)?);

        Ok(NonTerminal(fun_node))
    }

    fn var_decl(&mut self, var_token: Token) -> Result<Ast, LoxError> {
        let ident = self.consume(&vec![TokenType::Identifier])?;
        let mut decl_node =
            AstNode::new(AstType::VarDecl, Some(AstValue::Str(ident.lexeme.clone())));
        decl_node.add_child(Terminal(var_token));
        decl_node.add_child(Terminal(ident));
        let next_token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;
        match &next_token.token_type {
            TokenType::Semicolon => {
                decl_node.add_child(Terminal(next_token));
            }
            TokenType::Equal => {
                decl_node.add_child(Terminal(next_token));
                let init_expr = self.expression(None)?;
                decl_node.add_child(init_expr);
                let semicolon = self.consume(&vec![TokenType::Semicolon])?;
                decl_node.add_child(Terminal(semicolon));
            }
            _ => {
                return Err(Self::error("unexpected token type"));
            }
        }

        Ok(NonTerminal(decl_node))
    }

    fn statement(&mut self, token: Token) -> Result<Ast, LoxError> {
        match token.token_type {
            TokenType::If => self.if_stmt(token),
            TokenType::While => self.while_stmt(),
            TokenType::For => self.for_stmt(),
            TokenType::Print => self.print_stmt(token),
            TokenType::Return => self.return_stmt(),
            TokenType::LeftBrace => self.block(token),
            _ => self.expression_stmt(token),
        }
    }

    fn if_stmt(&mut self, if_token: Token) -> Result<Ast, LoxError> {
        let mut if_node = AstNode::new(AstType::IfStmt, None);
        if_node.add_child(Terminal(if_token));
        let open_paren = self.consume(&vec![TokenType::LeftParen])?;
        if_node.add_child(Terminal(open_paren));
        let cond_expr = self.expression(None)?;
        if_node.add_child(cond_expr);
        let close_paren = self.consume(&vec![TokenType::RightParen])?;
        if_node.add_child(Terminal(close_paren));
        let next_token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;
        let then_branch = self.statement(next_token)?;
        if_node.add_child(then_branch);

        let next_token = self.peek();
        if let Some(token) = next_token {
            if token.token_type == TokenType::Else {
                let else_token = self.advance()?.unwrap();
                if_node.add_child(Terminal(else_token));
                let next_token = self
                    .advance()?
                    .ok_or(Self::error("expected token, but got none"))?;
                let else_branch = self.statement(next_token)?;
                if_node.add_child(else_branch);
            }
        }

        Ok(NonTerminal(if_node))
    }

    fn while_stmt(&mut self) -> Result<Ast, LoxError> {
        let mut while_node = AstNode::new(AstType::WhileStmt, None);
        self.consume(&vec![TokenType::LeftParen])?;
        let condition = self.expression(None)?;
        while_node.add_child(condition);
        self.consume(&vec![TokenType::RightParen])?;
        let token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;
        while_node.add_child(self.statement(token)?);

        Ok(NonTerminal(while_node))
    }

    fn for_stmt(&mut self) -> Result<Ast, LoxError> {
        let mut for_node = AstNode::new(AstType::ForStmt, None);
        self.consume(&vec![TokenType::LeftParen])?;

        let mut token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;

        match &token.token_type {
            TokenType::Var => {
                let mut var_decl = self.var_decl(token)?;
                var_decl.set_label("initializer");
                for_node.add_child(var_decl);
            }
            TokenType::Semicolon => {}
            _ => {
                let mut expr_stmt = self.expression_stmt(token)?;
                expr_stmt.set_label("initializer");
                for_node.add_child(expr_stmt);
            }
        }

        token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;

        match &token.token_type {
            TokenType::Semicolon => {}
            _ => {
                let mut expr = self.expression(Some(token))?;
                expr.set_label("condition");
                for_node.add_child(expr);
                self.consume(&vec![TokenType::Semicolon])?;
            }
        }

        token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;

        match &token.token_type {
            TokenType::RightParen => {}
            _ => {
                let mut expr = self.expression(Some(token))?;
                expr.set_label("increment");
                for_node.add_child(expr);
                self.consume(&vec![TokenType::RightParen])?;
            }
        }

        token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;

        let mut stmt = self.statement(token)?;
        stmt.set_label("statement");
        for_node.add_child(stmt);

        Ok(NonTerminal(for_node))
    }

    fn print_stmt(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut print_node = AstNode::new(AstType::PrintStmt, None);
        print_node.add_child(Terminal(token));
        print_node.add_child(self.expression(None)?);
        let semicolon = self.consume(&vec![TokenType::Semicolon])?;
        print_node.add_child(Terminal(semicolon));

        Ok(NonTerminal(print_node))
    }

    fn return_stmt(&mut self) -> Result<Ast, LoxError> {
        let mut ret_node = AstNode::new(AstType::ReturnStmt, None);
        let next_token = self
            .advance()?
            .ok_or(Self::error("expected token, but got none"))?;
        if next_token.token_type != TokenType::Semicolon {
            ret_node.add_child(self.expression(Some(next_token))?);
            self.consume(&vec![TokenType::Semicolon])?;
        }

        Ok(NonTerminal(ret_node))
    }

    fn block(&mut self, left_brace: Token) -> Result<Ast, LoxError> {
        let mut block_node = AstNode::new(AstType::Block, None);
        block_node.add_child(Terminal(left_brace));

        loop {
            let next_token = match self.advance()? {
                Some(token) => token,
                None => return Err(Self::error("expected token, but got none")),
            };
            match &next_token.token_type {
                TokenType::RightBrace => {
                    block_node.add_child(Terminal(next_token));
                    break;
                }
                _ => {
                    block_node.add_child(self.declaration(next_token)?);
                }
            }
        }

        Ok(NonTerminal(block_node))
    }

    fn expression_stmt(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut stmt_node = AstNode::new(AstType::ExprStmt, None);
        stmt_node.add_child(self.expression(Some(token))?);
        let semicolon = self.consume(&vec![TokenType::Semicolon])?;
        stmt_node.add_child(Terminal(semicolon));

        Ok(NonTerminal(stmt_node))
    }

    pub fn expression(&mut self, start_token: Option<Token>) -> Result<Ast, LoxError> {
        let token = match start_token {
            Some(token) => token,
            None => self
                .advance()?
                .ok_or(Self::error("expected token but got none"))?,
        };

        self.assignment(token)
    }

    fn assignment(&mut self, token: Token) -> Result<Ast, LoxError> {
        let lhs = self.disjunction(token)?;
        
        let equal_token = if let Some(next_token) = self.peek() {
            if next_token.token_type == TokenType::Equal {
                self.consume(&vec![TokenType::Equal])?
            } else {
                return Ok(lhs);
            }
        } else {
            return Ok(lhs);
        };
        
        if !Self::is_valid_lhs(&lhs) {
            return Err(Self::error("invalid left-hand side in assignment"));
        }
        
        let mut assign_node = AstNode::new(AstType::Assignment, None);
        assign_node.add_child(lhs);
        assign_node.add_child(Terminal(equal_token));
        assign_node.add_child(self.expression(None)?);

        Ok(NonTerminal(assign_node))
    }
    
    fn is_valid_lhs(ast: &Ast) -> bool {
        match ast {  
            Terminal(_) => false,
            NonTerminal(node) => {
                match node.get_type() {
                    AstType::VarRef => true,
                    AstType::Binary => {
                        if let Some(value) = node.get_value_str() {
                            if value != "."{
                                return false;
                            }
                            let children = node.get_children();
                            if let Some(NonTerminal(child)) = children.get(2) {
                                if child.get_type() == &AstType::VarRef {
                                    return true;
                                }
                            }
                        }
                        false
                    }
                    _ => false,
                }
            }
        }
    }

    fn disjunction(&mut self, first_token: Token) -> Result<Ast, LoxError> {
        let mut token = first_token;
        let mut operands = Vec::new();

        loop {
            let operand = self.conjunction(token)?;
            operands.push(operand);
            match self.get_next_operand(&TokenType::Or)? {
                Some(tok) => {
                    token = tok;
                }
                None => break,
            }
        }

        if operands.len() == 1 {
            return Ok(operands.pop().unwrap());
        }

        let mut ret = AstNode::new(AstType::Disjunction, None);
        for operand in operands {
            ret.add_child(operand);
        }

        Ok(NonTerminal(ret))
    }

    fn conjunction(&mut self, first_token: Token) -> Result<Ast, LoxError> {
        let mut token = first_token;
        let mut operands = Vec::new();

        loop {
            let operand = self.equality(token)?;
            operands.push(operand);
            match self.get_next_operand(&TokenType::And)? {
                Some(tok) => {
                    token = tok;
                }
                None => break,
            }
        }

        if operands.len() == 1 {
            return Ok(operands.pop().unwrap());
        }

        let mut ret = AstNode::new(AstType::Conjunction, None);
        for operand in operands {
            ret.add_child(operand);
        }

        Ok(NonTerminal(ret))
    }

    fn get_next_operand(&mut self, operator_type: &TokenType) -> Result<Option<Token>, LoxError> {
        if let Some(tok) = self.peek() {
            if &tok.token_type == operator_type {
                self.advance()?;
                self.advance()?
                    .ok_or(Self::error("expected token but got none"))
                    .map(Some)
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn equality(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut operands = VecDeque::new();
        let mut operators = VecDeque::new();

        let mut next_token = token;

        loop {
            let operand = self.comparison(next_token)?;
            operands.push_back(operand);

            match self.peek() {
                Some(token) => match token.token_type {
                    TokenType::EqualEqual | TokenType::BangEqual => {}
                    _ => break,
                },
                None => break,
            }

            let operator = self.advance()?.unwrap();
            operators.push_back(operator);

            next_token = self
                .advance()?
                .ok_or(Self::error("expected operand but got none"))?;
        }

        Ok(Parser::left_assoc_bin_ast(&mut operands, &mut operators))
    }

    fn comparison(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut operands = VecDeque::new();
        let mut operators = VecDeque::new();

        let mut next_token = token;

        loop {
            let operand = self.sum(next_token)?;
            operands.push_back(operand);

            match self.peek() {
                Some(token) => match token.token_type {
                    TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual => {}
                    _ => break,
                },
                None => break,
            }

            let operator = self.advance()?.unwrap();
            operators.push_back(operator);

            next_token = self
                .advance()?
                .ok_or(Self::error("expected operand but got none"))?;
        }

        Ok(Parser::left_assoc_bin_ast(&mut operands, &mut operators))
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

            next_token = self
                .advance()?
                .ok_or(Self::error("expected operand but got none"))?;
        }

        Ok(Parser::left_assoc_bin_ast(&mut operands, &mut operators))
    }

    fn product(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut operands = VecDeque::new();
        let mut operators = VecDeque::new();

        let mut next_token = token;

        loop {
            let operand = self.path(next_token)?;
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

            next_token = self
                .advance()?
                .ok_or(Self::error("expected operand but got none"))?;
        }

        Ok(Parser::left_assoc_bin_ast(&mut operands, &mut operators))
    }

    fn path(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut operands = VecDeque::new();
        let mut operators = VecDeque::new();

        let mut next_token = token;

        loop {
            let operand = self.atom(next_token)?;
            operands.push_back(operand);

            match self.peek() {
                Some(token) => match token.token_type {
                    TokenType::Dot => {}
                    _ => break,
                },
                None => break,
            }

            let operator = self.advance()?.unwrap();
            operators.push_back(operator);

            next_token = self
                .advance()?
                .ok_or(Self::error("expected operand but got none"))?;
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
        let expr = match token.token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::Number
            | TokenType::Str => Ok(Terminal(token)),
            TokenType::Identifier => self.var_ref(token),
            TokenType::LeftParen => self.group(token),
            TokenType::Bang | TokenType::Minus => self.unary(token),
            _ => Err(Self::error("unexpected token")),
        }?;

        if let Some(token) = self.peek() {
            if token.token_type == TokenType::LeftParen {
                return self.call(expr);
            }
        }

        Ok(expr)
    }

    fn var_ref(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut var_ref_node =
            AstNode::new(AstType::VarRef, Some(AstValue::Str(token.clone().lexeme)));
        var_ref_node.add_child(Terminal(token));
        Ok(NonTerminal(var_ref_node))
    }

    fn call(&mut self, callee: Ast) -> Result<Ast, LoxError> {
        self.consume(&vec![TokenType::LeftParen])?;

        let mut call_node = AstNode::new(AstType::Call, None);
        call_node.add_child(callee);

        // Parse arguments
        loop {
            let token = self
                .advance()?
                .ok_or(Self::error("expected token but got none"))?;
            if token.token_type == TokenType::RightParen {
                break;
            }
            call_node.add_child(self.expression(Some(token))?);
            match self.peek() {
                Some(token) => {
                    if token.token_type == TokenType::Comma {
                        self.advance()?;
                    }
                }
                None => return Err(Self::error("expected token but got none")),
            }
        }

        match self.peek() {
            Some(token) if token.token_type == TokenType::LeftParen => {
                self.call(NonTerminal(call_node))
            }
            _ => Ok(NonTerminal(call_node)),
        }
    }

    fn group(&mut self, open_paren: Token) -> Result<Ast, LoxError> {
        let mut group_node = AstNode::new(AstType::Group, None);
        group_node.add_child(Terminal(open_paren));
        group_node.add_child(self.expression(None)?);
        group_node.add_child(Terminal(self.consume(&vec![TokenType::RightParen])?));

        Ok(NonTerminal(group_node))
    }

    fn unary(&mut self, operator: Token) -> Result<Ast, LoxError> {
        let mut unary_node = AstNode::new(AstType::Unary, None);
        unary_node.add_child(Terminal(operator));
        let next_token = self
            .advance()?
            .ok_or(Self::error("expected token but got none"))?;
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
        let next_token = self
            .token_stream
            .peek()
            .ok_or(Self::error("expected token but got none"))?;

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
            Err(Self::error(&format!(
                "token '{}' has unexpected type",
                next_token.lexeme
            )))
        }
    }

    fn error(msg: &str) -> LoxError {
        LoxError::new_in_parser_ctx(msg.to_string())
    }
}

pub fn parse_expression(code: &str) -> Result<Ast, LoxError> {
    let scanner = Scanner::new(CharStream::new(code.to_string()));
    let mut parser = Parser::new(scanner);
    parser.expression(None)
}

pub fn parse_program(code: &str) -> Result<Ast, LoxError> {
    let scanner = Scanner::new(CharStream::new(code.to_string()));
    let mut parser = Parser::new(scanner);
    parser.program()
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

    #[test]
    fn print_statement() {
        let result = parse_program(
            r#"
            print "Hallo Welt!";
            "#,
        );

        assert!(result.is_ok());
    }

    #[test]
    fn disjunction() {
        let result = parse_program(
            r#"
            var answer = 42;
            answer or 42;
            "#,
        );

        assert!(result.is_ok(), "ERROR: {}", result.err().unwrap());
    }

    #[test]
    fn for_stmt() {
        let result = parse_program(
            r#"
            for (var foo = 0; foo < 3;) print foo = foo + 1;
            "#,
        );

        assert!(result.is_ok(), "ERROR: {}", result.err().unwrap());
    }

    #[test]
    fn fun_decl() {
        let result = parse_program(
            r#"
            fun foo(a, b) {
                print a + b;
            }
            "#,
        );

        assert!(result.is_ok(), "ERROR: {}", result.err().unwrap());
    }

    #[test]
    fn fun_call() {
        let result = parse_program(
            r#"
            fun foo(a, b) {
                print a + b;
            }
            print foo(1, 2);
            "#,
        );

        assert!(result.is_ok(), "ERROR: {}", result.err().unwrap());
    }

    #[test]
    fn fun_call_error() {
        let result = parse_program(
            r#"
            fun foo(a, b c) {
                print a + b + c;
            }
            print foo(1, 2, 3);
            "#,
        );

        assert!(result.is_err());
    }

    #[test]
    fn var_resolution() {
        let code = r#"
        var a = "global";
        {
            fun showA() {
                print a;
            }

            showA();
            var a = "block";
            showA();
        }
        "#;

        let result = parse_program(code);

        assert!(result.is_ok());
    }
    
    #[test]
    fn param_resolution() {
        let code = r#"
        fun foo(a) {
            print a;
        }
        foo("hello");
        "#;

        let result = parse_program(code);

        assert!(result.is_ok());
    }

    #[test]
    fn field_setter() {
        let code = r#"
        class Spaceship {}
        
        var ship = Spaceship();
        ship.length = 42;
        "#;

        let result = parse_program(code);

        assert!(result.is_ok());
    }
}
