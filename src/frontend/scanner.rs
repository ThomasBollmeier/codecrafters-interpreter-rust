use std::error::Error;
use std::fmt::{Display, Formatter};
use anyhow::anyhow;
use crate::frontend::stream::{BufferedStream, CharStream, Stream};
use crate::frontend::tokens::{Token, TokenType};

pub struct Scanner {
    stream: BufferedStream<char, ()>,
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
    
    fn advance(&mut self) -> Result<Option<char>, Box<dyn Error>> {
        match self.stream.advance() {
            Ok(next_char_opt) => {
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
                Ok(next_char_opt)
            }
            Err(_) => Err(Box::from(anyhow!("Could not advance to next char")))
        }
    }
    
    fn create_token(&self, token_type: TokenType, lexeme: String) -> Token {
        Token::new(token_type, self.line, self.col, lexeme)
    }
}

impl Stream<Token, LexicalError> for Scanner {
    fn next(&mut self) -> Result<Option<Token>, LexicalError> {
        let line = self.line;
        let char = match self.advance() {
            Ok(char_opt) => match char_opt {
                Some(char) => char,
                None => return Ok(None),
            }
            Err(err) => {
                let message = format!("{err}");
                return Err(LexicalError::new(message, line));
            }
        };
        
        if let Some(token_type) = TokenType::get_single_char_token_type(char) {
            return Ok(Some(self.create_token(token_type, format!("{char}"))));
        }

        Err(LexicalError::new(
            format!("Unexpected character: {}", char),
            line))
    }
}

#[derive(Debug)]
pub struct LexicalError {
    message: String,
    line: usize,
}

impl LexicalError {
    fn new(message: String, line: usize) -> LexicalError {
        LexicalError {
            message,
            line
        }
    }

    pub fn get_line(&self) -> usize {
        self.line
    }
    
    pub fn get_message(&self) -> &str {
        &self.message
    }
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LexicalError {}