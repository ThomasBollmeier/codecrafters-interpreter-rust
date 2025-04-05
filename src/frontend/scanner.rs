use std::error::Error;
use std::fmt::{Display, Formatter};
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
    
    fn advance(&mut self) -> Result<Option<char>, LexicalError> {
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
            Err(_) => Err(LexicalError::new(
                "Could not advance to next char".to_string(),
                self.line
            ))
        }
    }
    
    fn create_token(&self, token_type: TokenType, lexeme: String) -> Token {
        Token::new(token_type, self.line, self.col, lexeme)
    }

    fn skip_line_comment(&mut self) -> Result<(usize, Option<char>), LexicalError> {
        loop {
            let char_opt = self.advance()?;
            match char_opt {
                Some('\n') => {
                    let line = self.line;
                    let char_opt = self.advance()?;
                    return Ok((line, char_opt));
                }
                Some(_) => {}
                None => {
                    return Ok((self.line, None));
                }
            }
        }
    }
}

impl Stream<Token, LexicalError> for Scanner {
    fn next(&mut self) -> Result<Option<Token>, LexicalError> {
        let mut line: usize;
        let mut char: char;
        loop {
            line = self.line;
            char = match self.advance()? {
                Some(char) => char,
                None => return Ok(None),
            };
            
            if char.is_whitespace() {
                continue;
            } else if char == '/' {
                let next_char = match self.stream.peek() {
                    Some(next_char) => next_char,
                    None => break,
                };
                if *next_char == '/' {
                    let (line_after_comment, char_opt) = self.skip_line_comment()?;
                    line = line_after_comment;
                    char = match char_opt {
                        Some(char) => char,
                        None => return Ok(None),
                    };
                }
                break;
            } else {
                break;
            }
        }
        
        if let Some(token_type) = TokenType::get_single_char_token_type(char) {
            return Ok(Some(self.create_token(token_type, format!("{char}"))));
        }

        match char {
            '=' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(self.create_token(TokenType::EqualEqual, "==".to_string())))
                    }
                    _ => Ok(Some(self.create_token(TokenType::Equal, "=".to_string())))
                }
            }
            '!' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(self.create_token(TokenType::BangEqual, "!=".to_string())))
                    }
                    _ => Ok(Some(self.create_token(TokenType::Bang, "!".to_string())))
                }
            }
            '<' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(self.create_token(TokenType::LessEqual, "<=".to_string())))
                    }
                    _ => Ok(Some(self.create_token(TokenType::Less, "<".to_string())))
                }
            }
            '>' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(self.create_token(TokenType::GreaterEqual, ">=".to_string())))
                    }
                    _ => Ok(Some(self.create_token(TokenType::Greater, ">".to_string())))
                }
            }
            _ => Err(LexicalError::new(
                format!("Unexpected character: {}", char),
                line)) 
        }
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