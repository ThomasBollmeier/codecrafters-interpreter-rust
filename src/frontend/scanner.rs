use crate::common::LoxError;
use crate::frontend::stream::{BufferedStream, CharStream, Stream};
use crate::frontend::tokens::{Literal, Token, TokenType};

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

    fn advance(&mut self) -> Result<Option<char>, LoxError> {
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
            Err(_) => Err(LoxError::new_in_lexical_ctx(
                "Could not advance to next char".to_string(),
                self.line,
            )),
        }
    }

    fn skip_line_comment(&mut self) -> Result<(), LoxError> {
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

    fn scan_one_or_two_char_operator(
        &mut self,
        line: usize,
        column: usize,
        ch: char,
        token_type_1: TokenType,
        token_type_2: TokenType,
    ) -> Result<Option<Token>, LoxError> {
        match self.stream.peek() {
            Some('=') => {
                self.advance()?;
                Ok(Some(Token::new(
                    token_type_2,
                    line,
                    column,
                    format!("{}=", ch),
                    Literal::Null,
                )))
            }
            _ => Ok(Some(Token::new(
                token_type_1,
                line,
                column,
                format!("{ch}"),
                Literal::Null,
            ))),
        }
    }

    fn scan_string(&mut self, line: usize, column: usize) -> Result<Option<Token>, LoxError> {
        let mut lexeme = String::from("\"");
        loop {
            match self.advance()? {
                Some(ch) => {
                    lexeme.push(ch);
                    if ch == '"' {
                        break;
                    }
                }
                None => {
                    return Err(LoxError::new_in_lexical_ctx(
                        "Unterminated string.".to_string(),
                        line,
                    ))
                }
            }
        }
        let s = lexeme[1..lexeme.len() - 1].to_string();
        Ok(Some(Token::new(
            TokenType::Str,
            line,
            column,
            lexeme,
            Literal::Str(s),
        )))
    }

    fn scan_number(
        &mut self,
        first_digit: char,
        line: usize,
        column: usize,
    ) -> Result<Option<Token>, LoxError> {
        let mut num_str = String::new();
        num_str.push(first_digit);

        let mut dot_encountered = false;
        let mut last_char_opt: Option<char> = None;

        while let Some(&next_char) = self.stream.peek() {
            if next_char.is_ascii_digit() || (next_char == '.' && !dot_encountered) {
                num_str.push(next_char);
                last_char_opt = Some(next_char);
                self.advance().expect("must not happen");
                dot_encountered = next_char == '.';
            } else {
                break;
            }
        }

        if let Some('.') = last_char_opt {
            return Err(LoxError::new_in_lexical_ctx(
                "invalid number".to_string(),
                line,
            ));
        }

        let num = num_str.parse::<f64>().unwrap();

        Ok(Some(Token::new(
            TokenType::Number,
            line,
            column,
            num_str,
            Literal::Number(num),
        )))
    }

    fn starts_identifier(ch: char) -> bool {
        ch == '_' || ch.is_alphabetic()
    }

    fn is_valid_identifier_char(ch: char) -> bool {
        ch == '_' || ch.is_alphanumeric()
    }

    fn scan_identifier(
        &mut self,
        first_char: char,
        line: usize,
        column: usize,
    ) -> Result<Option<Token>, LoxError> {
        let mut ident = String::from(first_char);

        while let Some(&next_char) = self.stream.peek() {
            if !Scanner::is_valid_identifier_char(next_char) {
                break;
            }
            self.advance()?;
            ident.push(next_char);
        }

        let token_type = TokenType::get_keyword_token_type(&ident).unwrap_or(TokenType::Identifier);

        Ok(Some(Token::new(
            token_type,
            line,
            column,
            ident.clone(),
            Literal::Null,
        )))
    }
}

impl Stream<Token, LoxError> for Scanner {
    fn next(&mut self) -> Result<Option<Token>, LoxError> {
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
            return Ok(Some(Token::new(
                token_type,
                line,
                column,
                format!("{char}"),
                Literal::Null,
            )));
        }

        match char {
            '=' => self.scan_one_or_two_char_operator(
                line,
                column,
                char,
                TokenType::Equal,
                TokenType::EqualEqual,
            ),
            '!' => self.scan_one_or_two_char_operator(
                line,
                column,
                char,
                TokenType::Bang,
                TokenType::BangEqual,
            ),
            '<' => self.scan_one_or_two_char_operator(
                line,
                column,
                char,
                TokenType::Less,
                TokenType::LessEqual,
            ),
            '>' => self.scan_one_or_two_char_operator(
                line,
                column,
                char,
                TokenType::Greater,
                TokenType::GreaterEqual,
            ),
            '"' => self.scan_string(line, column),
            ch if ch.is_ascii_digit() => self.scan_number(ch, line, column),
            ch if Scanner::starts_identifier(ch) => self.scan_identifier(ch, line, column),
            _ => Err(LoxError::new_in_lexical_ctx(
                format!("Unexpected character: {}", char),
                line,
            )),
        }
    }
}
