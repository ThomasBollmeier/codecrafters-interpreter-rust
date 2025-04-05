use std::fmt::{Display, Formatter};
use TokenType::*;

#[derive(Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Equal,
    EqualEqual,
    Eof,
}

impl TokenType {
    pub fn get_single_char_token_type(ch: char) -> Option<TokenType> {
        match ch {
            '(' => Some(LeftParen),
            ')' => Some(RightParen),
            '{' => Some(LeftBrace),
            '}' => Some(RightBrace),
            ',' => Some(Comma),
            '.' => Some(Dot),
            '+' => Some(Plus),
            '-' => Some(Minus),
            ';' => Some(Semicolon),
            '/' => Some(Slash),
            '*' => Some(Star),
            _ => None
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let text = match &self {  
            LeftParen => "LEFT_PAREN".to_string(),
            RightParen => "RIGHT_PAREN".to_string(),
            LeftBrace => "LEFT_BRACE".to_string(),
            RightBrace => "RIGHT_BRACE".to_string(),
            Comma => "COMMA".to_string(),
            Dot => "DOT".to_string(),
            Minus => "MINUS".to_string(),
            Plus => "PLUS".to_string(),
            Semicolon => "SEMICOLON".to_string(),
            Slash => "SLASH".to_string(),
            Star => "STAR".to_string(),
            Equal => "EQUAL".to_string(),
            EqualEqual => "EQUAL_EQUAL".to_string(),
            Eof => "EOF".to_string(),
        };
        write!(f, "{}", text)
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
    pub lexeme: String,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, column: usize, lexeme: String) -> Token {
        Token {
            token_type,
            line,
            column,
            lexeme,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} null", self.token_type, self.lexeme)
    }
}