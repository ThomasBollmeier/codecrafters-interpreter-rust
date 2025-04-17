use std::fmt::{Display, Formatter};
use TokenType::*;

#[derive(Debug, Eq, PartialEq, Clone)]
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
    Identifier,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
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
            _ => None,
        }
    }

    pub fn get_keyword_token_type(lexeme: &str) -> Option<TokenType> {
        match lexeme {
            "and" => Some(And),
            "class" => Some(Class),
            "else" => Some(Else),
            "false" => Some(False),
            "for" => Some(For),
            "fun" => Some(Fun),
            "if" => Some(If),
            "nil" => Some(Nil),
            "or" => Some(Or),
            "print" => Some(Print),
            "return" => Some(Return),
            "super" => Some(Super),
            "this" => Some(This),
            "true" => Some(True),
            "var" => Some(Var),
            "while" => Some(While),
            _ => None,
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
            Identifier => "IDENTIFIER".to_string(),
            And => "AND".to_string(),
            Class => "CLASS".to_string(),
            Else => "ELSE".to_string(),
            False => "FALSE".to_string(),
            For => "FOR".to_string(),
            Fun => "FUN".to_string(),
            If => "IF".to_string(),
            Nil => "NIL".to_string(),
            Or => "OR".to_string(),
            Print => "PRINT".to_string(),
            Return => "RETURN".to_string(),
            Super => "SUPER".to_string(),
            This => "THIS".to_string(),
            True => "TRUE".to_string(),
            Var => "VAR".to_string(),
            While => "WHILE".to_string(),
            Eof => "EOF".to_string(),
        };
        write!(f, "{}", text)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Null,
    Str(String),
    Number(f64),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
    pub lexeme: String,
    pub literal: Literal,
}

impl Token {
    pub fn new(
        token_type: TokenType,
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
            literal,
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
