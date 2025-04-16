use crate::common::LoxError;
use crate::frontend::ast::Ast::{NonTerminal, Terminal};
use crate::frontend::ast::{Ast, AstNode, AstType};
use crate::frontend::scanner::Scanner;
use crate::frontend::stream::{BufferedStream, CharStream};
use crate::frontend::tokens::{Token, TokenType};
use std::collections::VecDeque;
use std::vec;

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

        Ok(NonTerminal(program_node))
    }

    fn declaration(&mut self, token: Token) -> Result<Ast, LoxError> {
        match token.token_type {
            TokenType::Var => self.var_decl(token),
            _ => self.statement(token),
        }
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
            TokenType::Print => self.print_stmt(token),
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
        let stmt = self.statement(token)?;
        while_node.add_child(stmt);
        
        Ok(NonTerminal(while_node))
    }

    fn print_stmt(&mut self, token: Token) -> Result<Ast, LoxError> {
        let mut print_node = AstNode::new(AstType::PrintStmt, None);
        print_node.add_child(Terminal(token));
        print_node.add_child(self.expression(None)?);
        let semicolon = self.consume(&vec![TokenType::Semicolon])?;
        print_node.add_child(Terminal(semicolon));

        Ok(NonTerminal(print_node))
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
        let is_valid_lhs = &token.token_type == &TokenType::Identifier;
        if !is_valid_lhs {
            return self.disjunction(token);
        }
        let equal_token = if let Some(next_token) = self.peek() {
            if next_token.token_type == TokenType::Equal {
                self.consume(&vec![TokenType::Equal])?
            } else {
                return self.disjunction(token);
            }
        } else {
            return self.disjunction(token);
        };
        let mut assign_node = AstNode::new(AstType::Assignment, None);
        assign_node.add_child(Terminal(token));
        assign_node.add_child(Terminal(equal_token));
        assign_node.add_child(self.expression(None)?);

        Ok(NonTerminal(assign_node))
    }

    fn disjunction(&mut self, first_token: Token) -> Result<Ast, LoxError> {
        let mut token = first_token;
        let mut operands = Vec::new();

        loop {
            let operand = self.conjunction(token)?;
            operands.push(operand);
            if let Some(tok) = self.peek() {
                if tok.token_type == TokenType::Or {
                    self.advance()?;
                    token = self
                        .advance()?
                        .ok_or(Self::error("expected token but got none"))?;
                } else {
                    break;
                }
            } else {
                break;
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
            if let Some(tok) = self.peek() {
                if tok.token_type == TokenType::And {
                    self.advance()?;
                    token = self
                        .advance()?
                        .ok_or(Self::error("expected token but got none"))?;
                } else {
                    break;
                }
            } else {
                break;
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
        match token.token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::Number
            | TokenType::Str
            | TokenType::Identifier => Ok(Terminal(token)),
            TokenType::LeftParen => self.group(token),
            TokenType::Bang | TokenType::Minus => self.unary(token),
            _ => Err(Self::error("unexpected token")),
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
}
