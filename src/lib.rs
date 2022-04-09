use std::collections::HashMap;
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
    Id,
    Assign,
    Begin,
    End,
    Semi,
    Dot,
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
    reserved_keywords: HashMap<String, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut reserved_keywords = HashMap::new();
        reserved_keywords.insert(String::from("BEGIN"), Token::new(TokenKind::Begin, None));
        reserved_keywords.insert(String::from("END"), Token::new(TokenKind::End, None));
        Lexer {
            stream: input.chars().peekable(),
            reserved_keywords,
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
        let mut integer = String::new();

        while let Some(current_char) = self.stream.peek().copied() {
            if !current_char.is_digit(10) {
                break;
            }
            integer.push(current_char);
            self.advance();
        }

        integer
    }

    fn id(&mut self) -> Token {
        let mut id = String::new();
        while let Some(current_char) = self.stream.peek().copied() {
            if !current_char.is_alphanumeric() {
                break;
            }
            id.push(current_char);
            self.advance();
        }

        match self.reserved_keywords.get(&id) {
            None => Token::new(TokenKind::Id, Some(id)),
            Some(token) => Token::new(token.kind.clone(), token.value.clone()),
        }
    }

    fn get_next_token(&mut self) -> Token {
        while let Some(current_char) = self.stream.peek().copied() {
            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char.is_alphabetic() {
                return self.id();
            }

            if current_char.is_digit(10) {
                return Token::new(TokenKind::Integer, Some(self.integer()));
            }

            if current_char == ':' {
                self.advance();
                return match self.stream.peek().copied() {
                    Some(c) if c == '=' => {
                        self.advance();
                        Token::new(TokenKind::Assign, None)
                    }
                    Some(c) => panic!("Invalid character. Character: {}", c),
                    None => panic!("Invalid character. Character: None"),
                };
            }

            if current_char == ';' {
                self.advance();
                return Token::new(TokenKind::Semi, None);
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

            if current_char == '.' {
                self.advance();
                return Token::new(TokenKind::Dot, None);
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
    Compound,
    Assign,
    Var,
    NoOp,
}

pub trait Ast<'a> {
    fn node(&self) -> Node;
    fn token(&self) -> Option<&Token>;
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

struct Compound<'a> {
    children: Vec<Box<NodeType<'a>>>,
}

struct Assign<'a> {
    token: Token,
    left: Box<NodeType<'a>>,
    right: Box<NodeType<'a>>,
}

struct Var {
    token: Token,
    value: String,
}

struct NoOp {}

impl<'a> BinOp<'a> {
    fn new(left: Box<NodeType<'a>>, token: Token, right: Box<NodeType<'a>>) -> Self {
        BinOp { token, left, right }
    }
}

impl<'a> Ast<'a> for BinOp<'a> {
    fn node(&self) -> Node {
        Node::BinOp
    }

    fn token(&self) -> Option<&Token> {
        Some(&self.token)
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
    fn token(&self) -> Option<&Token> {
        Some(&self.token)
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

    fn token(&self) -> Option<&Token> {
        Some(&self.token)
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.expr]
    }
}

impl<'a> Compound<'a> {
    fn new(children: Vec<Box<NodeType<'a>>>) -> Self {
        Compound { children }
    }
}

impl<'a> Ast<'a> for Compound<'a> {
    fn node(&self) -> Node {
        Node::Compound
    }

    fn token(&self) -> Option<&Token> {
        None
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        self.children.iter().map(|n| &**n).collect()
    }
}

impl<'a> Assign<'a> {
    fn new(left: Box<NodeType<'a>>, token: Token, right: Box<NodeType<'a>>) -> Self {
        Assign { left, token, right }
    }
}

impl<'a> Ast<'a> for Assign<'a> {
    fn node(&self) -> Node {
        Node::Assign
    }

    fn token(&self) -> Option<&Token> {
        Some(&self.token)
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.left, &*self.right]
    }
}

impl Var {
    fn new(token: Token) -> Self {
        let value = token.value.clone().unwrap();
        Var { token, value }
    }
}

impl<'a> Ast<'a> for Var {
    fn node(&self) -> Node {
        Node::Var
    }
    fn token(&self) -> Option<&Token> {
        Some(&self.token)
    }
    fn value(&self) -> Option<&str> {
        Some(&self.value)
    }
    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![]
    }
}

impl NoOp {
    fn new() -> Self {
        NoOp {}
    }
}

impl<'a> Ast<'a> for NoOp {
    fn node(&self) -> Node {
        Node::NoOp
    }
    fn token(&self) -> Option<&Token> {
        None
    }
    fn value(&self) -> Option<&str> {
        None
    }
    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![]
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

    fn program(&mut self) -> Box<NodeType<'a>> {
        let node = self.compound_statement();
        self.eat(TokenKind::Dot).unwrap();
        node
    }

    fn compound_statement(&mut self) -> Box<NodeType<'a>> {
        self.eat(TokenKind::Begin).unwrap();
        let nodes = self.statement_list();
        self.eat(TokenKind::End).unwrap();
        Box::new(Compound::new(nodes))
    }

    fn statement_list(&mut self) -> Vec<Box<NodeType<'a>>> {
        let mut result = vec![self.statement()];

        while self.current_token.kind == TokenKind::Semi {
            self.eat(TokenKind::Semi).unwrap();
            result.push(self.statement());
        }

        result
    }

    fn statement(&mut self) -> Box<NodeType<'a>> {
        match self.current_token.kind {
            TokenKind::Begin => self.compound_statement(),
            TokenKind::Id => self.assignment_statement(),
            _ => self.empty(),
        }
    }

    fn assignment_statement(&mut self) -> Box<NodeType<'a>> {
        let left = self.variable();
        let token = Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        );
        self.eat(TokenKind::Assign).unwrap();

        Box::new(Assign::new(left, token, self.expr()))
    }

    fn variable(&mut self) -> Box<NodeType<'a>> {
        let node = Var::new(Token::new(
            self.current_token.kind.clone(),
            self.current_token.value.clone(),
        ));
        self.eat(TokenKind::Id).unwrap();

        Box::new(node)
    }

    fn empty(&mut self) -> Box<NodeType<'a>> {
        Box::new(NoOp::new())
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
            TokenKind::Id => self.variable(),
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
        let node = self.program();
        if self.current_token.kind != TokenKind::Eof {
            panic!("Invalid token. Token: {:?}", self.current_token);
        }

        node
    }
}

// /////////////////////////////////////////////////////////// //
// INTERPRETER                                                 //
// /////////////////////////////////////////////////////////// //

pub struct Interpreter {
    pub global_scope: HashMap<String, String>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_scope: HashMap::new(),
        }
    }

    fn visit(&mut self, node: &NodeType<'a>) {
        match node.node() {
            Node::Compound => self.visit_compound(node),
            Node::Assign => self.visit_assign(node),
            Node::NoOp => self.visit_no_op(),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        };
    }

    fn visit_expr(&self, node: &NodeType<'a>) -> i32 {
        match node.node() {
            Node::Num => self.visit_num(node),
            Node::BinOp => self.visit_bin_op(node),
            Node::UnaryOp => self.visit_unary_op(node),
            Node::Var => self.visit_var(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        }
    }

    fn visit_bin_op(&self, node: &NodeType<'a>) -> i32 {
        let childrens = node.children();
        match node.token().unwrap().kind {
            TokenKind::Plus => self.visit_expr(childrens[0]) + self.visit_expr(childrens[1]),
            TokenKind::Minus => self.visit_expr(childrens[0]) - self.visit_expr(childrens[1]),
            TokenKind::Mul => self.visit_expr(childrens[0]) * self.visit_expr(childrens[1]),
            TokenKind::Div => self.visit_expr(childrens[0]) / self.visit_expr(childrens[1]),
            _ => panic!("Invalid op. Op: {:?}", node.token()),
        }
    }

    fn visit_num(&self, node: &NodeType<'a>) -> i32 {
        node.value().unwrap().parse::<i32>().unwrap()
    }

    fn visit_unary_op(&self, node: &NodeType<'a>) -> i32 {
        match node.token().unwrap().kind {
            TokenKind::Plus => self.visit_expr(node.children()[0]),
            TokenKind::Minus => -self.visit_expr(node.children()[0]),
            _ => panic!("Invalid op. Op: {:?}", node.token()),
        }
    }

    fn visit_compound(&mut self, node: &NodeType<'a>) {
        for children in node.children() {
            self.visit(children);
        }
    }

    fn visit_assign(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        self.global_scope.insert(
            children[0].token().unwrap().value.clone().unwrap(),
            self.visit_expr(children[1]).to_string(),
        );
    }

    fn visit_var(&self, node: &NodeType<'a>) -> i32 {
        match self
            .global_scope
            .get(&(node.token().unwrap().value.clone().unwrap()))
        {
            Some(var_value) => var_value.parse::<i32>().unwrap(),
            None => panic!("No Value for var {:?}", node.token()),
        }
    }

    fn visit_no_op(&self) {}

    pub fn interpret(&mut self, tree: &NodeType<'a>) {
        self.visit(tree)
    }
}
