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
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Str,
    Number,
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
            Bang => "BANG".to_string(),
            BangEqual => "BANG_EQUAL".to_string(),
            Less => "LESS".to_string(),
            LessEqual => "LESS_EQUAL".to_string(),
            Greater => "GREATER".to_string(),
            GreaterEqual => "GREATER_EQUAL".to_string(),
            Str => "STRING".to_string(),
            Number => "NUMBER".to_string(),
            Eof => "EOF".to_string(),
        };
        write!(f, "{}", text)
    }
}

#[derive(Debug)]
pub enum Literal {
    Null,
    Str(String),
    Number(f64),
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
    pub lexeme: String,
    pub literal: Literal,
}

impl Token {
    pub fn new(token_type: TokenType, 
               line: usize, 
               column: usize, 
               lexeme: String,
               literal: Literal,
    ) -> Token {
        Token {
            token_type,
            line,
            column,
            lexeme,
            literal
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let literal = match &self.literal {
            Literal::Str(s) => s,
            Literal::Null => "null",
            Literal::Number(x) => &format!("{x:?}"),
        };
        write!(f, "{} {} {}", self.token_type, self.lexeme, literal)
    }
}