use crate::frontend::stream::CharStream;
use crate::frontend::tokens::{Token, TokenType};

pub struct Scanner {
    char_stream: CharStream,
    line: usize,
    col: usize, 
}

impl Scanner {
    pub fn new(char_stream: CharStream) -> Scanner {
        Scanner {
            char_stream,
            line: 0, 
            col: 0,
        }
    }
}

impl Iterator for Scanner {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let char = self.char_stream.next();
        if char.is_none() {
            return None;
        }
        let char = char.unwrap();
        match char {
            '(' => Some(Token::new(TokenType::LeftParen, self.line, self.col, format!("{char}"))),
            ')' => Some(Token::new(TokenType::RightParen, self.line, self.col, format!("{char}"))),
            _ => None,
        }
    }
}