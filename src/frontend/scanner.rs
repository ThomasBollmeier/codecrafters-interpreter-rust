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

    fn skip_line_comment(&mut self) -> Result<(), LexicalError> {
        loop {
            let char_opt = self.advance()?;
            match char_opt {
                Some('\n') => {
                    return Ok(());
                }
                Some(_) => {}
                None => {
                    return Ok(());
                }
            }
        }
    }

    fn scan_string(&mut self, line: usize, column: usize) -> Result<Option<Token>, LexicalError> {
        let mut lexeme = String::from("\"");
        loop {
            match self.advance()? {
                Some(ch) => {
                    lexeme.push(ch);
                    if ch == '"' {
                        break;
                    }
                }
                None => return Err(LexicalError::new(
                    "Unterminated string.".to_string(),
                    line))
            }
        }
        Ok(Some(Token::new(TokenType::Str, line, column, lexeme)))
    }
}

impl Stream<Token, LexicalError> for Scanner {
    fn next(&mut self) -> Result<Option<Token>, LexicalError> {
        let mut line: usize;
        let mut column: usize;
        let mut char: char;
        loop {
            line = self.line;
            column = self.col;
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
                    self.skip_line_comment()?;
                    continue;
                }
                break;
            } else {
                break;
            }
        }
        
        if let Some(token_type) = TokenType::get_single_char_token_type(char) {
            return Ok(Some(Token::new(token_type, line, column, format!("{char}"))));
        }

        match char {
            '=' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(Token::new(TokenType::EqualEqual, line, column, "==".to_string())))
                    }
                    _ => Ok(Some(Token::new(TokenType::Equal, line, column, "=".to_string())))
                }
            }
            '!' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(Token::new(TokenType::BangEqual, line, column, "!=".to_string())))
                    }
                    _ => Ok(Some(Token::new(TokenType::Bang, line, column, "!".to_string())))
                }
            }
            '<' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(Token::new(TokenType::LessEqual, line, column, "<=".to_string())))
                    }
                    _ => Ok(Some(Token::new(TokenType::Less, line, column, "<".to_string())))
                }
            }
            '>' => {
                match self.stream.peek() {
                    Some('=') => {
                        self.advance()?;
                        Ok(Some(Token::new(TokenType::GreaterEqual, line, column, ">=".to_string())))
                    }
                    _ => Ok(Some(Token::new(TokenType::Greater, line, column, ">".to_string())))
                }
            }
            '"' => self.scan_string(line, column),
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