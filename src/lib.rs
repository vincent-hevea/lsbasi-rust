use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
enum TokenKind {
    Integer,
    Plus,
    Minus,
    Mul,
    Div,
    Root,
    Eof,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    value: Option<String>,
}

impl Token {
    fn new(kind: TokenKind, value: Option<String>) -> Self {
        Token { kind, value }
    }
}

pub struct Lexer<'a> {
    stream: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            stream: input.chars().peekable(),
        }
    }

    fn advance(&mut self) {
        self.stream.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(current_char) = self.stream.peek().copied() {
            if !current_char.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn integer(&mut self) -> String {
        let mut c_integer = String::new();

        while let Some(current_char) = self.stream.peek().copied() {
            if !current_char.is_digit(10) {
                break;
            }
            c_integer.push(current_char);
            self.advance();
        }

        c_integer
    }

    fn get_next_token(&mut self) -> Token {
        while let Some(current_char) = self.stream.peek().copied() {
            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char.is_digit(10) {
                return Token::new(TokenKind::Integer, Some(self.integer()));
            }

            if current_char == '+' {
                self.advance();
                return Token::new(TokenKind::Plus, Some(current_char.to_string()));
            }

            if current_char == '-' {
                self.advance();
                return Token::new(TokenKind::Minus, Some(current_char.to_string()));
            }

            if current_char == '*' {
                self.advance();
                return Token::new(TokenKind::Mul, Some(current_char.to_string()));
            }

            if current_char == '/' {
                self.advance();
                return Token::new(TokenKind::Div, Some(current_char.to_string()));
            }

            panic!("Invalid character. Character: {}", current_char);
        }

        Token::new(TokenKind::Eof, None)
    }
}

pub struct Interpreter<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Interpreter<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Interpreter {
            lexer,
            current_token: Token::new(TokenKind::Root, None),
        }
    }

    fn eat(&mut self, token_kind: TokenKind) -> Result<(), String> {
        if self.current_token.kind == token_kind {
            self.current_token = self.lexer.get_next_token();
            Ok(())
        } else {
            Err(format!(
                "Unable to eat. Expected token kind: {:?}. Actual token: {:?}",
                token_kind, self.current_token
            ))
        }
    }

    fn factor(&mut self) -> i32 {
        let token = Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        );
        self.eat(TokenKind::Integer).unwrap();

        token.value.as_ref().unwrap().parse::<i32>().unwrap()
    }

    fn term(&mut self) -> i32 {
        let mut result = self.factor();

        while [TokenKind::Mul, TokenKind::Div].contains(&self.current_token.kind) {
            if self.current_token.kind == TokenKind::Mul {
                self.eat(TokenKind::Mul).unwrap();
                result *= self.factor();
            } else {
                self.eat(TokenKind::Div).unwrap();
                result /= self.factor();
            }
        }

        result
    }

    pub fn expr(&mut self) -> i32 {
        self.eat(TokenKind::Root).unwrap();

        let mut result = self.term();

        while [TokenKind::Plus, TokenKind::Minus].contains(&self.current_token.kind) {
            if self.current_token.kind == TokenKind::Plus {
                self.eat(TokenKind::Plus).unwrap();
                result += self.term();
            } else {
                self.eat(TokenKind::Minus).unwrap();
                result -= self.term();
            }
        }

        result
    }
}
