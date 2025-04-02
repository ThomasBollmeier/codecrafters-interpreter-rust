use crate::frontend::stream::{BufferedStream, CharStream};
use crate::frontend::tokens::{Token, TokenType};

pub struct Scanner {
    stream: BufferedStream<char>,
    line: usize,
    col: usize, 
}

impl Scanner {
    pub fn new(char_stream: CharStream) -> Scanner {
        Scanner {
            stream: BufferedStream::new(Box::new(char_stream)),
            line: 1, 
            col: 1,
        }
    }
    
    fn advance(&mut self) -> Option<char> {
        let next_char_opt = self.stream.advance();
        match next_char_opt {
            Some('\n') => {
                self.line += 1;
                self.col = 1;
                
            }
            Some(_) => {
                self.col += 1;
            }
            None => {}
        }
        next_char_opt
    }
    
    fn create_token(&self, token_type: TokenType, lexeme: String) -> Token {
        Token::new(token_type, self.line, self.col, lexeme)
    }
}

impl Iterator for Scanner {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let char = self.advance();
        if char.is_none() {
            return None;
        }
        let char = char.unwrap();
        match char {
            '(' => Some(self.create_token(TokenType::LeftParen, format!("{char}"))),
            ')' => Some(self.create_token(TokenType::RightParen, format!("{char}"))),
            '{' => Some(self.create_token(TokenType::LeftBrace, format!("{char}"))),
            '}' => Some(self.create_token(TokenType::RightBrace, format!("{char}"))),
            _ => None,
        }
    }
}