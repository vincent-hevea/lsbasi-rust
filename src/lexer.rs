use crate::error::{Error, ErrorCategory, ErrorKind};
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    IntegerConst,
    RealConst,
    Integer,
    Real,
    Plus,
    Minus,
    Mul,
    IntegerDiv,
    FloatDiv,
    Lparen,
    Rparen,
    Program,
    Procedure,
    Var,
    Id,
    Assign,
    Begin,
    End,
    Semi,
    Colon,
    Comma,
    Dot,
    Root,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    kind: TokenKind,
    value: String,
    line: Option<i32>,
    col: Option<i32>,
}

impl Token {
    pub fn new(kind: TokenKind, value: String) -> Self {
        Token {
            kind,
            value,
            line: None,
            col: None,
        }
    }

    fn new_localized(kind: TokenKind, value: String, line: i32, col: i32) -> Token {
        Token {
            kind,
            value,
            line: Some(line),
            col: Some(col),
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn line(&self) -> Option<i32> {
        self.line
    }

    pub fn col(&self) -> Option<i32> {
        self.col
    }
}

pub struct Lexer<'a> {
    stream: Peekable<Chars<'a>>,
    reserved_keywords: HashMap<String, Token>,
    line: i32,
    col: i32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut reserved_keywords = HashMap::new();
        reserved_keywords.insert(
            String::from("PROGRAM"),
            Token::new(TokenKind::Program, "PROGRAM".to_string()),
        );
        reserved_keywords.insert(
            String::from("VAR"),
            Token::new(TokenKind::Var, "VAR".to_string()),
        );
        reserved_keywords.insert(
            String::from("PROCEDURE"),
            Token::new(TokenKind::Procedure, "PROCEDURE".to_string()),
        );
        reserved_keywords.insert(
            String::from("DIV"),
            Token::new(TokenKind::IntegerDiv, "DIV".to_string()),
        );
        reserved_keywords.insert(
            String::from("INTEGER"),
            Token::new(TokenKind::Integer, String::from("INTEGER")),
        );
        reserved_keywords.insert(
            String::from("REAL"),
            Token::new(TokenKind::Real, String::from("REAL")),
        );
        reserved_keywords.insert(
            String::from("BEGIN"),
            Token::new(TokenKind::Begin, "BEGIN".to_string()),
        );
        reserved_keywords.insert(
            String::from("END"),
            Token::new(TokenKind::End, "END".to_string()),
        );
        Lexer {
            stream: input.chars().peekable(),
            reserved_keywords,
            line: 1,
            col: 1,
        }
    }

    fn advance(&mut self) {
        self.stream.next();
        if let Some(current_char) = self.stream.peek() {
            if *current_char == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(current_char) = self.stream.peek().copied() {
            if !current_char.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn skip_comment(&mut self) {
        while let Some(current_char) = self.stream.peek().copied() {
            if current_char == '}' {
                break;
            }
            self.advance();
        }
        self.advance();
    }

    fn number(&mut self) -> Token {
        let mut number = String::new();
        let mut token_kind = TokenKind::IntegerConst;
        let line = self.line;
        let col = self.col;

        while let Some(current_char) = self.stream.peek().copied() {
            if current_char == '.' && number.matches('.').count() > 0 {
                panic!(
                    "{}",
                    Error::new(
                        "Invalid number".to_string(),
                        ErrorKind::InvalidNumber,
                        Some(self.line),
                        Some(self.col),
                        current_char.to_string(),
                        ErrorCategory::Lexer
                    )
                );
            }
            if current_char == '.' {
                number.push('.');
                token_kind = TokenKind::RealConst;
                self.advance();
                continue;
            }
            if !current_char.is_digit(10) {
                break;
            }
            number.push(current_char);
            self.advance();
        }

        Token::new_localized(token_kind, number, line, col)
    }

    fn id(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut id = String::new();
        while let Some(current_char) = self.stream.peek().copied() {
            if !current_char.is_alphanumeric() {
                break;
            }
            id.push(current_char);
            self.advance();
        }

        match self.reserved_keywords.get(&id.to_uppercase()) {
            None => Token::new_localized(TokenKind::Id, id, line, col),
            Some(token) => {
                Token::new_localized(token.kind().clone(), token.value().to_string(), line, col)
            }
        }
    }

    pub fn get_next_token(&mut self) -> Token {
        while let Some(current_char) = self.stream.peek().copied() {
            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char == '{' {
                self.advance();
                self.skip_comment();
                continue;
            }

            if current_char.is_alphabetic() {
                return self.id();
            }

            if current_char.is_digit(10) {
                return self.number();
            }

            if current_char == ':' {
                let line = self.line;
                let col = self.col;
                self.advance();
                return match self.stream.peek().copied() {
                    Some(c) if c == '=' => {
                        self.advance();
                        Token::new_localized(TokenKind::Assign, ":=".to_string(), line, col)
                    }
                    Some(_) => Token::new_localized(TokenKind::Colon, ":".to_string(), line, col),
                    None => panic!(
                        "{}",
                        Error::new(
                            "Invalid character".to_string(),
                            ErrorKind::UnexpectedCharacter,
                            Some(line),
                            Some(col),
                            current_char.to_string(),
                            ErrorCategory::Lexer
                        )
                    ),
                };
            }

            let mut simple_char_token_kind = None;
            if current_char == ',' {
                simple_char_token_kind = Some(TokenKind::Comma);
            }

            if current_char == ';' {
                simple_char_token_kind = Some(TokenKind::Semi);
            }

            if current_char == '+' {
                simple_char_token_kind = Some(TokenKind::Plus);
            }

            if current_char == '-' {
                simple_char_token_kind = Some(TokenKind::Minus);
            }

            if current_char == '*' {
                simple_char_token_kind = Some(TokenKind::Mul);
            }

            if current_char == '/' {
                simple_char_token_kind = Some(TokenKind::FloatDiv);
            }

            if current_char == '(' {
                simple_char_token_kind = Some(TokenKind::Lparen);
            }

            if current_char == ')' {
                simple_char_token_kind = Some(TokenKind::Rparen);
            }

            if current_char == '.' {
                simple_char_token_kind = Some(TokenKind::Dot);
            }

            if let Some(token_kind) = simple_char_token_kind {
                return self.simple_char_token(token_kind, current_char);
            }

            panic!(
                "{}",
                Error::new(
                    "Invalid character".to_string(),
                    ErrorKind::UnexpectedCharacter,
                    Some(self.line),
                    Some(self.col),
                    current_char.to_string(),
                    ErrorCategory::Lexer
                )
            );
        }

        Token::new_localized(TokenKind::Eof, "".to_string(), self.line, self.col)
    }

    fn simple_char_token(&mut self, token_kind: TokenKind, current_char: char) -> Token {
        let line = self.line;
        let col = self.col;
        self.advance();

        Token::new_localized(token_kind, current_char.to_string(), line, col)
    }
}
