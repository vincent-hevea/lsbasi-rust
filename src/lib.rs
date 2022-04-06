use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
enum TokenKind {
    Integer,
    Plus,
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

    /// Lexical analyzer (also known as scanner or tokenizer)
    ///
    /// This method is responsible for breaking a sentence apart into tokens. One token at a time.
    fn get_next_token(&mut self) -> Token {
        let current_char = self.stream.peek().copied();

        match current_char {
            Some(x) if x.is_digit(10) => {
                self.stream.next();
                Token::new(TokenKind::Integer, Some(x.to_string()))
            }
            Some(x) if x == '+' => {
                self.stream.next();
                Token::new(TokenKind::Plus, Some(x.to_string()))
            }
            None => {
                self.stream.next();
                Token::new(TokenKind::Eof, None)
            }
            Some(x) => {
                self.stream.next();
                panic!("Invalid character. Character: {}", x);
            }
        }
    }

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

    pub fn expr(&mut self) -> i32 {
        self.eat(TokenKind::Root).unwrap();

        let token_left = Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        );
        self.eat(TokenKind::Integer).unwrap();

        self.eat(TokenKind::Plus).unwrap();

        let token_right = Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        );
        self.eat(TokenKind::Integer).unwrap();

        token_left.value.as_ref().unwrap().parse::<i32>().unwrap()
            + token_right.value.as_ref().unwrap().parse::<i32>().unwrap()
    }
}
