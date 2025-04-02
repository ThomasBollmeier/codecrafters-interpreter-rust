use std::fmt::{Display, Formatter};

pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let text = match &self {  
            TokenType::LeftParen => "LEFT_PAREN".to_string(),
            TokenType::RightParen => "RIGHT_PAREN".to_string(),
            TokenType::LeftBrace => "LEFT_BRACE".to_string(),
            TokenType::RightBrace => "RIGHT_BRACE".to_string(),
            TokenType::Eof => "EOF".to_string(),
        };
        write!(f, "{}", text)
    }
}

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