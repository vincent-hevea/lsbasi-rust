use std::iter::Peekable;
use std::str::Chars;

type NodeType<'a> = dyn Ast<'a> + 'a;

// /////////////////////////////////////////////////////////// //
// LEXER                                                       //
// /////////////////////////////////////////////////////////// //

#[derive(PartialEq, Clone, Debug)]
enum TokenKind {
    Integer,
    Plus,
    Minus,
    Mul,
    Div,
    Lparen,
    Rparen,
    Root,
    Eof,
}

#[derive(Debug)]
pub struct Token {
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

            if current_char == '(' {
                self.advance();
                return Token::new(TokenKind::Lparen, Some(current_char.to_string()));
            }

            if current_char == ')' {
                self.advance();
                return Token::new(TokenKind::Rparen, Some(current_char.to_string()));
            }

            panic!("Invalid character. Character: {}", current_char);
        }

        Token::new(TokenKind::Eof, None)
    }
}

// /////////////////////////////////////////////////////////// //
// PARSER                                                      //
// /////////////////////////////////////////////////////////// //

#[derive(Debug)]
pub enum Node {
    BinOp,
    Num,
    UnaryOp,
}

pub trait Ast<'a> {
    fn node(&self) -> Node;
    fn token(&self) -> &Token;
    fn value(&self) -> Option<&str>;
    fn children(&self) -> Vec<&NodeType<'a>>;
}

struct BinOp<'a> {
    token: Token,
    left: Box<NodeType<'a>>,
    right: Box<NodeType<'a>>,
}

struct Num {
    token: Token,
    value: String,
}

struct UnaryOp<'a> {
    token: Token,
    expr: Box<NodeType<'a>>,
}

impl<'a> BinOp<'a> {
    fn new(left: Box<NodeType<'a>>, token: Token, right: Box<NodeType<'a>>) -> Self {
        BinOp { token, left, right }
    }
}

impl<'a> Ast<'a> for BinOp<'a> {
    fn node(&self) -> Node {
        Node::BinOp
    }

    fn token(&self) -> &Token {
        &self.token
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.left, &*self.right]
    }
}

impl Num {
    fn new(token: Token) -> Self {
        let value = token.value.clone().unwrap();
        Num { token, value }
    }
}

impl<'a> Ast<'a> for Num {
    fn node(&self) -> Node {
        Node::Num
    }
    fn token(&self) -> &Token {
        &self.token
    }
    fn value(&self) -> Option<&str> {
        Some(&self.value)
    }
    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![]
    }
}

impl<'a> UnaryOp<'a> {
    fn new(token: Token, expr: Box<NodeType<'a>>) -> Self {
        UnaryOp { token, expr }
    }
}

impl<'a> Ast<'a> for UnaryOp<'a> {
    fn node(&self) -> Node {
        Node::UnaryOp
    }

    fn token(&self) -> &Token {
        &self.token
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.expr]
    }
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        Parser {
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

    fn factor(&mut self) -> Box<NodeType<'a>> {
        let token = Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        );

        match self.current_token.kind {
            TokenKind::Integer => {
                self.eat(TokenKind::Integer).unwrap();
                Box::new(Num::new(token))
            }
            TokenKind::Lparen => {
                self.eat(TokenKind::Lparen).unwrap();
                let node = self.expr();
                self.eat(TokenKind::Rparen).unwrap();
                node
            }
            TokenKind::Plus => {
                self.eat(TokenKind::Plus).unwrap();
                Box::new(UnaryOp::new(token, self.factor()))
            }
            TokenKind::Minus => {
                self.eat(TokenKind::Minus).unwrap();
                Box::new(UnaryOp::new(token, self.factor()))
            }
            _ => panic!("Invalid token. Token: {:?}", self.current_token),
        }
    }

    fn term(&mut self) -> Box<NodeType<'a>> {
        let mut node = self.factor();
        let mut current_token = Token {
            kind: self.current_token.kind.clone(),
            value: self.current_token.value.clone(),
        };

        while [TokenKind::Mul, TokenKind::Div].contains(&self.current_token.kind) {
            if current_token.kind == TokenKind::Mul {
                self.eat(TokenKind::Mul).unwrap();
            } else {
                self.eat(TokenKind::Div).unwrap();
            }
            node = Box::new(BinOp::new(node, current_token, self.factor()));
            current_token = Token {
                kind: self.current_token.kind.clone(),
                value: self.current_token.value.clone(),
            };
        }

        node
    }

    fn expr(&mut self) -> Box<NodeType<'a>> {
        let mut node = self.term();
        let mut current_token = Token {
            kind: self.current_token.kind.clone(),
            value: self.current_token.value.clone(),
        };

        while [TokenKind::Plus, TokenKind::Minus].contains(&self.current_token.kind) {
            if self.current_token.kind == TokenKind::Plus {
                self.eat(TokenKind::Plus).unwrap();
            } else {
                self.eat(TokenKind::Minus).unwrap();
            }

            node = Box::new(BinOp::new(node, current_token, self.term()));
            current_token = Token {
                kind: self.current_token.kind.clone(),
                value: self.current_token.value.clone(),
            };
        }

        node
    }

    pub fn parse(&'a mut self) -> Box<NodeType<'a>> {
        self.eat(TokenKind::Root).unwrap();

        self.expr()
    }
}

// /////////////////////////////////////////////////////////// //
// INTERPRETER                                                 //
// /////////////////////////////////////////////////////////// //

pub struct Interpreter {}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    fn visit(&self, node: &NodeType<'a>) -> i32 {
        match node.node() {
            Node::Num => self.visit_num(node),
            Node::BinOp => self.visit_bin_op(node),
            Node::UnaryOp => self.visit_unary_op(node),
        }
    }

    fn visit_bin_op(&self, node: &NodeType<'a>) -> i32 {
        let childrens = node.children();
        match node.token().kind {
            TokenKind::Plus => self.visit(childrens[0]) + self.visit(childrens[1]),
            TokenKind::Minus => self.visit(childrens[0]) - self.visit(childrens[1]),
            TokenKind::Mul => self.visit(childrens[0]) * self.visit(childrens[1]),
            TokenKind::Div => self.visit(childrens[0]) / self.visit(childrens[1]),
            _ => panic!("Invalid op. Op: {:?}", node.token()),
        }
    }

    fn visit_num(&self, node: &NodeType<'a>) -> i32 {
        node.value().unwrap().parse::<i32>().unwrap()
    }

    fn visit_unary_op(&self, node: &NodeType<'a>) -> i32 {
        match node.token().kind {
            TokenKind::Plus => self.visit(node.children()[0]),
            TokenKind::Minus => -self.visit(node.children()[0]),
            _ => panic!("Invalid op. Op: {:?}", node.token()),
        }
    }

    pub fn interpret(&self, tree: &NodeType<'a>) -> i32 {
        self.visit(tree)
    }
}
