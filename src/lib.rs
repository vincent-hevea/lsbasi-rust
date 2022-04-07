use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
enum TokenKind {
    Integer,
    Plus,
    Minus,
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

pub struct Interpreter<'a> {
    stream: Peekable<Chars<'a>>,
    current_token: Token,
}

impl<'a> Interpreter<'a> {
    pub fn new(input: &'a str) -> Self {
        Interpreter {
            stream: input.chars().peekable(),
            current_token: Token::new(TokenKind::Root, None),
        }
    }

    // /////////////////////////////////////////////////////////// //
    // Lexer code                                                  //
    // /////////////////////////////////////////////////////////// //
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

    /// Lexical analyzer (also known as scanner or tokenizer)
    ///
    /// This method is responsible for breaking a sentence apart into tokens.
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

            panic!("Invalid character. Character: {}", current_char);
        }

        Token::new(TokenKind::Eof, None)
    }

    // /////////////////////////////////////////////////////////// //
    // Parser / Interpreter code                                   //
    // /////////////////////////////////////////////////////////// //
    fn eat(&mut self, token_kind: TokenKind) -> Result<(), String> {
        if self.current_token.kind == token_kind {
            self.current_token = self.get_next_token();
            Ok(())
        } else {
            Err(format!(
                "Unable to eat. Expected token kind: {:?}. Actual token: {:?}",
                token_kind, self.current_token
            ))
        }
    }

    fn term(&mut self) -> i32 {
        let token = Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        );
        self.eat(TokenKind::Integer).unwrap();

        token.value.as_ref().unwrap().parse::<i32>().unwrap()
    }

    /// Parser / Interpreter
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
