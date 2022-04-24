use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

type NodeType<'a> = dyn Ast<'a> + 'a;

// /////////////////////////////////////////////////////////// //
// LEXER                                                       //
// /////////////////////////////////////////////////////////// //

#[derive(PartialEq, Clone, Debug)]
enum TokenKind {
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
        reserved_keywords.insert(
            String::from("PROGRAM"),
            Token::new(TokenKind::Program, None),
        );
        reserved_keywords.insert(String::from("VAR"), Token::new(TokenKind::Var, None));
        reserved_keywords.insert(
            String::from("PROCEDURE"),
            Token::new(TokenKind::Procedure, None),
        );
        reserved_keywords.insert(String::from("DIV"), Token::new(TokenKind::IntegerDiv, None));
        reserved_keywords.insert(
            String::from("INTEGER"),
            Token::new(TokenKind::Integer, Some(String::from("INTEGER"))),
        );
        reserved_keywords.insert(
            String::from("REAL"),
            Token::new(TokenKind::Real, Some(String::from("REAL"))),
        );
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

        while let Some(current_char) = self.stream.peek().copied() {
            if current_char == '.' && number.matches('.').count() > 0 {
                panic!("Invalid number");
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

        Token::new(token_kind, Some(number))
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

        match self.reserved_keywords.get(&id.to_uppercase()) {
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
                self.advance();
                return match self.stream.peek().copied() {
                    Some(c) if c == '=' => {
                        self.advance();
                        Token::new(TokenKind::Assign, None)
                    }
                    Some(_) => Token::new(TokenKind::Colon, None),
                    None => panic!("Invalid character. Character: None"),
                };
            }

            if current_char == ',' {
                self.advance();
                return Token::new(TokenKind::Comma, None);
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
                return Token::new(TokenKind::FloatDiv, Some(current_char.to_string()));
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

#[derive(PartialEq, Debug)]
pub enum Node {
    BinOp,
    Num,
    UnaryOp,
    Program,
    Procedure,
    Params,
    Block,
    Compound,
    Assign,
    Var,
    VarDecl,
    Type,
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

struct Program<'a> {
    name: String,
    block: Box<NodeType<'a>>,
}

struct Block<'a> {
    declarations: Vec<Box<NodeType<'a>>>,
    compound_statement: Box<NodeType<'a>>,
}

struct VarDecl<'a> {
    var_node: Box<NodeType<'a>>,
    type_node: Box<NodeType<'a>>,
}

struct Type {
    token: Token,
    value: String,
}

struct Params<'a> {
    var_node: Box<NodeType<'a>>,
    type_node: Box<NodeType<'a>>,
}

struct ProcedureDecl<'a> {
    name: String,
    params: Vec<Box<NodeType<'a>>>,
    block: Box<NodeType<'a>>,
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

impl<'a> Program<'a> {
    fn new(name: String, block: Box<NodeType<'a>>) -> Self {
        Program { name, block }
    }
}

impl<'a> Ast<'a> for Program<'a> {
    fn node(&self) -> Node {
        Node::Program
    }

    fn token(&self) -> Option<&Token> {
        None
    }

    fn value(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.block]
    }
}

impl<'a> Block<'a> {
    fn new(declarations: Vec<Box<NodeType<'a>>>, compound_statement: Box<NodeType<'a>>) -> Self {
        Block {
            declarations,
            compound_statement,
        }
    }
}

impl<'a> Ast<'a> for Block<'a> {
    fn node(&self) -> Node {
        Node::Block
    }

    fn token(&self) -> Option<&Token> {
        None
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        let mut children: Vec<&NodeType<'a>> = vec![];
        children.extend(
            self.declarations
                .iter()
                .map(|n| &**n)
                .collect::<Vec<&NodeType<'a>>>(),
        );
        children.push(&*self.compound_statement);

        children
    }
}

impl<'a> VarDecl<'a> {
    fn new(var_node: Box<NodeType<'a>>, type_node: Box<NodeType<'a>>) -> Self {
        VarDecl {
            var_node,
            type_node,
        }
    }
}

impl<'a> Ast<'a> for VarDecl<'a> {
    fn node(&self) -> Node {
        Node::VarDecl
    }

    fn token(&self) -> Option<&Token> {
        None
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.var_node, &*self.type_node]
    }
}

impl Type {
    fn new(token: Token) -> Self {
        let value = token.value.clone().unwrap();
        Type { token, value }
    }
}

impl<'a> Ast<'a> for Type {
    fn node(&self) -> Node {
        Node::Type
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

impl<'a> Params<'a> {
    fn new(var_node: Box<NodeType<'a>>, type_node: Box<NodeType<'a>>) -> Self {
        Params {
            var_node,
            type_node,
        }
    }
}

impl<'a> Ast<'a> for Params<'a> {
    fn node(&self) -> Node {
        Node::Params
    }

    fn token(&self) -> Option<&Token> {
        None
    }

    fn value(&self) -> Option<&str> {
        None
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        vec![&*self.var_node, &*self.type_node]
    }
}

impl<'a> ProcedureDecl<'a> {
    fn new(name: String, params: Vec<Box<NodeType<'a>>>, block: Box<NodeType<'a>>) -> Self {
        ProcedureDecl {
            name,
            params,
            block,
        }
    }
}

impl<'a> Ast<'a> for ProcedureDecl<'a> {
    fn node(&self) -> Node {
        Node::Procedure
    }

    fn token(&self) -> Option<&Token> {
        None
    }

    fn value(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        let mut children: Vec<&NodeType<'a>> = self.params.iter().map(|n| &**n).collect();
        children.push(&*self.block);

        children
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
        self.eat(TokenKind::Program).unwrap();
        let name = String::from(self.variable().value().unwrap());
        self.eat(TokenKind::Semi).unwrap();
        let block = self.block();
        self.eat(TokenKind::Dot).unwrap();

        Box::new(Program::new(name, block))
    }

    fn block(&mut self) -> Box<NodeType<'a>> {
        Box::new(Block::new(self.declarations(), self.compound_statement()))
    }

    fn declarations(&mut self) -> Vec<Box<NodeType<'a>>> {
        let mut declarations: Vec<Box<NodeType<'a>>> = Vec::new();
        while self.current_token.kind == TokenKind::Var {
            self.eat(TokenKind::Var).unwrap();
            while self.current_token.kind == TokenKind::Id {
                declarations.append(&mut self.variable_declaration());
                self.eat(TokenKind::Semi).unwrap();
            }
        }
        while self.current_token.kind == TokenKind::Procedure {
            self.eat(TokenKind::Procedure).unwrap();
            let proc_name = self.current_token.value.clone().unwrap();
            self.eat(TokenKind::Id).unwrap();
            let mut params: Vec<Box<NodeType>> = Vec::new();
            if self.current_token.kind == TokenKind::Lparen {
                self.eat(TokenKind::Lparen).unwrap();
                params = self.formal_parameter_list();
                self.eat(TokenKind::Rparen).unwrap();
            }
            self.eat(TokenKind::Semi).unwrap();
            let block_node = self.block();
            declarations.push(Box::new(ProcedureDecl::new(proc_name, params, block_node)));
            self.eat(TokenKind::Semi).unwrap();
        }

        declarations
    }

    fn formal_parameters(&mut self) -> Vec<Box<NodeType<'a>>> {
        let mut tokens = vec![self.current_token.clone()];
        self.eat(TokenKind::Id).unwrap();

        while self.current_token.kind == TokenKind::Comma {
            self.eat(TokenKind::Comma).unwrap();
            tokens.push(self.current_token.clone());
            self.eat(TokenKind::Id).unwrap();
        }
        self.eat(TokenKind::Colon).unwrap();
        let type_node = self.type_spec();
        let mut param_nodes: Vec<Box<NodeType<'a>>> = vec![];
        let mut param_node: Box<NodeType<'a>>;
        for token in tokens.into_iter() {
            param_node = Box::new(Params::new(
                Box::new(Var::new(token)),
                Box::new(Type::new((*type_node).token().unwrap().clone())),
            ));
            param_nodes.push(param_node);
        }

        param_nodes
    }

    fn formal_parameter_list(&mut self) -> Vec<Box<NodeType<'a>>> {
        if self.current_token.kind != TokenKind::Id {
            return Vec::new();
        }

        let mut param_nodes = self.formal_parameters();
        while self.current_token.kind == TokenKind::Semi {
            self.eat(TokenKind::Semi).unwrap();
            param_nodes.append(&mut self.formal_parameters());
        }

        param_nodes
    }

    fn variable_declaration(&mut self) -> Vec<Box<NodeType<'a>>> {
        let mut tokens = vec![self.current_token.clone()];
        self.eat(TokenKind::Id).unwrap();

        while self.current_token.kind == TokenKind::Comma {
            self.eat(TokenKind::Comma).unwrap();
            tokens.push(self.current_token.clone());
            self.eat(TokenKind::Id).unwrap();
        }
        self.eat(TokenKind::Colon).unwrap();
        let type_node = self.type_spec();
        let mut declarations: Vec<Box<NodeType<'a>>> = vec![];
        let mut var_decl: Box<NodeType<'a>>;
        for token in tokens.into_iter() {
            var_decl = Box::new(VarDecl::new(
                Box::new(Var::new(token)),
                Box::new(Type::new((*type_node).token().unwrap().clone())),
            ));
            declarations.push(var_decl);
        }

        declarations
    }

    fn type_spec(&mut self) -> Box<NodeType<'a>> {
        let token = self.current_token.clone();
        if self.current_token.kind == TokenKind::Integer {
            self.eat(TokenKind::Integer).unwrap();
        } else {
            self.eat(TokenKind::Real).unwrap();
        }

        Box::new(Type::new(token))
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
        let token = self.current_token.clone();
        self.eat(TokenKind::Assign).unwrap();

        Box::new(Assign::new(left, token, self.expr()))
    }

    fn variable(&mut self) -> Box<NodeType<'a>> {
        let node = Var::new(self.current_token.clone());
        self.eat(TokenKind::Id).unwrap();

        Box::new(node)
    }

    fn empty(&mut self) -> Box<NodeType<'a>> {
        Box::new(NoOp::new())
    }

    fn factor(&mut self) -> Box<NodeType<'a>> {
        let token = self.current_token.clone();

        match self.current_token.kind {
            TokenKind::Plus => {
                self.eat(TokenKind::Plus).unwrap();
                Box::new(UnaryOp::new(token, self.factor()))
            }
            TokenKind::Minus => {
                self.eat(TokenKind::Minus).unwrap();
                Box::new(UnaryOp::new(token, self.factor()))
            }
            TokenKind::IntegerConst => {
                self.eat(TokenKind::IntegerConst).unwrap();
                Box::new(Num::new(token))
            }
            TokenKind::RealConst => {
                self.eat(TokenKind::RealConst).unwrap();
                Box::new(Num::new(token))
            }
            TokenKind::Lparen => {
                self.eat(TokenKind::Lparen).unwrap();
                let node = self.expr();
                self.eat(TokenKind::Rparen).unwrap();
                node
            }
            TokenKind::Id => self.variable(),
            _ => panic!("Invalid token. Token: {:?}", self.current_token),
        }
    }

    fn term(&mut self) -> Box<NodeType<'a>> {
        let mut node = self.factor();
        let mut current_token = self.current_token.clone();

        while [TokenKind::Mul, TokenKind::IntegerDiv, TokenKind::FloatDiv]
            .contains(&self.current_token.kind)
        {
            match current_token.kind {
                TokenKind::Mul => self.eat(TokenKind::Mul).unwrap(),
                TokenKind::IntegerDiv => self.eat(TokenKind::IntegerDiv).unwrap(),
                TokenKind::FloatDiv => self.eat(TokenKind::FloatDiv).unwrap(),
                _ => panic!("Invalid op: {:?}", current_token),
            }

            node = Box::new(BinOp::new(node, current_token, self.factor()));
            current_token = self.current_token.clone();
        }

        node
    }

    fn expr(&mut self) -> Box<NodeType<'a>> {
        let mut node = self.term();
        let mut current_token = self.current_token.clone();

        while [TokenKind::Plus, TokenKind::Minus].contains(&self.current_token.kind) {
            if self.current_token.kind == TokenKind::Plus {
                self.eat(TokenKind::Plus).unwrap();
            } else {
                self.eat(TokenKind::Minus).unwrap();
            }

            node = Box::new(BinOp::new(node, current_token, self.term()));
            current_token = self.current_token.clone();
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
// SYMBOLS, TABLES, SEMANTIC ANALYSIS                          //
// /////////////////////////////////////////////////////////// //

#[derive(Debug, Clone)]
enum KindSymbol {
    Integer,
    Real,
}

#[derive(Debug)]
struct VarSymbol {
    name: String,
    kind: KindSymbol,
}

impl VarSymbol {
    fn new(name: String, kind: KindSymbol) -> Self {
        VarSymbol { name, kind }
    }
}

#[derive(Debug)]
struct ProcedureSymbol {
    name: String,
    params: Vec<VarSymbol>,
}

impl ProcedureSymbol {
    fn new(name: String, params: Vec<VarSymbol>) -> Self {
        ProcedureSymbol { name, params }
    }
}

#[derive(Debug)]
enum Symbol {
    Kind(KindSymbol),
    Var(VarSymbol),
    Procedure(ProcedureSymbol),
}

#[derive(Debug)]
struct ScopedSymbolTable {
    symbols: HashMap<String, Symbol>,
    scope_name: String,
    scope_level: i32,
}

impl ScopedSymbolTable {
    fn new(scope_name: String, scope_level: i32) -> Self {
        let mut symbols = HashMap::new();
        symbols.insert("INTEGER".to_string(), Symbol::Kind(KindSymbol::Integer));
        symbols.insert("REAL".to_string(), Symbol::Kind(KindSymbol::Real));
        ScopedSymbolTable {
            symbols,
            scope_name,
            scope_level,
        }
    }

    fn insert(&mut self, symbol: Symbol) {
        match symbol {
            Symbol::Kind(matched_symbol) => match matched_symbol {
                KindSymbol::Integer => self
                    .symbols
                    .insert("INTEGER".to_string(), Symbol::Kind(KindSymbol::Integer)),
                KindSymbol::Real => self
                    .symbols
                    .insert("REAL".to_string(), Symbol::Kind(KindSymbol::Real)),
            },
            Symbol::Var(matched_symbol) => self
                .symbols
                .insert(matched_symbol.name.clone(), Symbol::Var(matched_symbol)),
            Symbol::Procedure(matched_symbol) => self.symbols.insert(
                matched_symbol.name.clone(),
                Symbol::Procedure(matched_symbol),
            ),
        };
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug)]
pub struct ScopedSymbolTableStack {
    stack: Vec<ScopedSymbolTable>,
}

impl ScopedSymbolTableStack {
    fn new() -> Self {
        ScopedSymbolTableStack { stack: Vec::new() }
    }

    fn lookup(&self, name: &str) -> Option<(&Symbol, i32)> {
        for scope in self.stack.iter().rev() {
            if let Some(symbol) = scope.lookup(name) {
                return Some((symbol, scope.scope_level));
            }
        }

        None
    }

    fn lookup_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.stack.last().and_then(|scope| scope.lookup(name))
    }
}

pub struct SemanticAnalyzer {
    pub scopes: ScopedSymbolTableStack,
}

impl<'a> Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            scopes: ScopedSymbolTableStack::new(),
        }
    }

    pub fn visit(&mut self, node: &NodeType<'a>) {
        match node.node() {
            Node::Compound => self.visit_compound(node),
            Node::Assign => self.visit_assign(node),
            Node::NoOp => self.visit_no_op(),
            Node::Program => self.visit_program(node),
            Node::Block => self.visit_block(node),
            Node::VarDecl => self.visit_var_decl(node),
            Node::Type => self.visit_type(),
            Node::Num => self.visit_num(),
            Node::BinOp => self.visit_bin_op(node),
            Node::UnaryOp => self.visit_unary_op(node),
            Node::Var => self.visit_var(node),
            Node::Procedure => self.visit_procedure_decl(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        };
    }

    fn visit_program(&mut self, node: &NodeType<'a>) {
        self.scopes
            .stack
            .push(ScopedSymbolTable::new("global".to_string(), 1));
        println!("{:#?}", self.scopes);
        self.visit(node.children()[0]);
        self.scopes.stack.pop();
    }

    fn visit_block(&mut self, node: &NodeType<'a>) {
        node.children()
            .into_iter()
            .for_each(|node_elt| self.visit(node_elt));
    }

    fn visit_var_decl(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        let symbol_name = String::from(children[0].value().unwrap());
        let node_type = children[1].value().unwrap();
        let symbol = self.scopes.lookup(node_type).unwrap().0;
        let symbol_type = self.symbol_type(symbol);

        if self
            .scopes
            .lookup_current_scope(children[0].token().unwrap().value.as_ref().unwrap())
            .is_some()
        {
            panic!(
                "Duplicate identifier: {}",
                children[0].token().unwrap().value.as_ref().unwrap()
            )
        }

        self.scopes
            .stack
            .last_mut()
            .unwrap()
            .insert(Symbol::Var(VarSymbol::new(symbol_name, symbol_type)));
        println!("{:#?}", self.scopes);
    }

    fn visit_type(&self) {}

    fn visit_bin_op(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        self.visit(children[0]);
        self.visit(children[1]);
    }

    fn visit_num(&self) {}

    fn visit_unary_op(&mut self, node: &NodeType<'a>) {
        self.visit(node.children()[0])
    }

    fn visit_compound(&mut self, node: &NodeType<'a>) {
        for children in node.children() {
            self.visit(children);
        }
    }

    fn visit_assign(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        self.visit(children[0]);
        self.visit(children[1]);
    }

    fn visit_var(&self, node: &NodeType<'a>) {
        if self.scopes.lookup(node.value().unwrap()).is_none() {
            panic!("Not in SymbolTable: {}", node.value().unwrap())
        }
    }

    fn visit_no_op(&self) {}

    fn visit_procedure_decl(&mut self, node: &NodeType<'a>) {
        let mut procedure_symbol =
            ProcedureSymbol::new(String::from(node.value().unwrap()), Vec::new());
        let mut new_scope = ScopedSymbolTable::new(
            String::from(node.value().unwrap()),
            self.scopes.stack.len() as i32 + 1,
        );
        for child in node.children() {
            if child.node() == Node::Params {
                let symbol = self
                    .scopes
                    .lookup(child.children()[1].value().unwrap())
                    .unwrap()
                    .0;
                let symbol_type = self.symbol_type(symbol);
                new_scope.insert(Symbol::Var(VarSymbol::new(
                    child.children()[0].value().unwrap().to_string(),
                    symbol_type,
                )));
                let symbol_type = self.symbol_type(symbol);
                procedure_symbol.params.push(VarSymbol::new(
                    child.children()[0].value().unwrap().to_string(),
                    symbol_type,
                ));
            }
        }
        self.scopes
            .stack
            .last_mut()
            .unwrap()
            .insert(Symbol::Procedure(procedure_symbol));
        self.scopes.stack.push(new_scope);
        println!("{:#?}", self.scopes);
        self.visit(*node.children().last().unwrap());
        self.scopes.stack.pop();
        println!("{:#?}", self.scopes);
    }

    fn symbol_type(&self, symbol: &Symbol) -> KindSymbol {
        match symbol {
            Symbol::Kind(matched_symbol) => matched_symbol.clone(),
            _ => panic!("No support for this Symbol: {:?}", symbol),
        }
    }
}

// /////////////////////////////////////////////////////////// //
// SOURCE-TO-SOURCE COMPILER                                   //
// /////////////////////////////////////////////////////////// //

pub struct SourceToSourceCompiler {
    scopes: ScopedSymbolTableStack,
    output: String,
}

impl<'a> Default for SourceToSourceCompiler {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> SourceToSourceCompiler {
    pub fn new() -> Self {
        SourceToSourceCompiler {
            scopes: ScopedSymbolTableStack::new(),
            output: String::new(),
        }
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn visit(&mut self, node: &NodeType<'a>) {
        match node.node() {
            Node::Block => self.visit_block(node),
            Node::Program => self.visit_program(node),
            Node::Compound => self.visit_compound(node),
            Node::NoOp => self.visit_no_op(),
            Node::BinOp => self.visit_bin_op(node),
            Node::Procedure => self.visit_procedure_decl(node),
            Node::VarDecl => self.visit_var_decl(node),
            Node::Assign => self.visit_assign(node),
            Node::Var => self.visit_var(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        };
    }

    fn visit_block(&mut self, node: &NodeType<'a>) {
        for child in node.children() {
            if child.node() != Node::Compound {
                self.visit(child)
            }
        }
        self.output.push_str("\nbegin\n");
        self.visit(*node.children().last().unwrap());
        self.output.push_str("\nend");
    }

    fn visit_program(&mut self, node: &NodeType<'a>) {
        self.output
            .push_str(&format!("program {}0;\n", node.value().unwrap()));
        self.scopes
            .stack
            .push(ScopedSymbolTable::new("global".to_string(), 1));
        self.visit(node.children()[0]);
        self.output.push('.');
        self.output
            .push_str(&format!(" {{END OF {}}}", node.value().unwrap()));
        self.scopes.stack.pop();
    }

    fn visit_compound(&mut self, node: &NodeType<'a>) {
        for children in node.children() {
            self.visit(children);
        }
    }

    fn visit_no_op(&self) {}

    fn visit_bin_op(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        self.visit(children[0]);
        let op_display = match node.token().unwrap().kind {
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Mul => "*",
            TokenKind::IntegerDiv => "DIV",
            TokenKind::FloatDiv => "/",
            _ => panic!("Invalid op. Op: {:?}", node.token()),
        };
        self.output.push_str(&format!(" {} ", op_display));
        self.visit(children[1]);
    }

    fn visit_procedure_decl(&mut self, node: &NodeType<'a>) {
        let mut procedure_symbol =
            ProcedureSymbol::new(String::from(node.value().unwrap()), Vec::new());
        let mut new_scope = ScopedSymbolTable::new(
            String::from(node.value().unwrap()),
            self.scopes.stack.len() as i32 + 1,
        );
        self.output.push_str(&format!(
            "\nprocedure {}{}",
            String::from(node.value().unwrap()),
            self.scopes.stack.last().unwrap().scope_level
        ));

        let mut param_detected = false;
        for child in node.children() {
            if child.node() == Node::Params {
                let mut sep = "; ";
                if !param_detected {
                    self.output.push('(');
                    param_detected = true;
                    sep = "";
                }
                self.output.push_str(sep);
                let node_type = child.children()[1].value().unwrap();
                let symbol_name = child.children()[0].value().unwrap();
                let symbol = self.scopes.lookup(node_type).unwrap().0;
                let symbol_type = self.symbol_type(symbol);
                new_scope.insert(Symbol::Var(VarSymbol::new(
                    child.children()[0].value().unwrap().to_string(),
                    symbol_type,
                )));
                self.output.push_str(&format!(
                    "{}{} : {}",
                    symbol_name, new_scope.scope_level, node_type
                ));
                let symbol_type = self.symbol_type(symbol);
                procedure_symbol.params.push(VarSymbol::new(
                    child.children()[0].value().unwrap().to_string(),
                    symbol_type,
                ));
            }
        }
        if param_detected {
            self.output.push(')');
        }
        self.output.push(';');
        self.output.push('\n');
        self.scopes
            .stack
            .last_mut()
            .unwrap()
            .insert(Symbol::Procedure(procedure_symbol));
        self.scopes.stack.push(new_scope);
        self.visit(*node.children().last().unwrap());
        self.output.push_str(&format!(
            "; {{END OF {}}}\n",
            String::from(node.value().unwrap())
        ));
        self.scopes.stack.pop();
    }

    fn visit_var_decl(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        let node_type = children[1].value().unwrap();
        let symbol = self.scopes.lookup(node_type).unwrap().0;
        let symbol_type = self.symbol_type(symbol);

        if self
            .scopes
            .lookup_current_scope(children[0].token().unwrap().value.as_ref().unwrap())
            .is_some()
        {
            panic!(
                "Duplicate identifier: {}",
                children[0].token().unwrap().value.as_ref().unwrap()
            )
        }

        self.scopes
            .stack
            .last_mut()
            .unwrap()
            .insert(Symbol::Var(VarSymbol::new(
                String::from(children[0].value().unwrap()),
                symbol_type,
            )));
        self.output.push_str(&format!(
            "var {}{} : {};\n",
            String::from(children[0].value().unwrap()),
            self.scopes.stack.last().unwrap().scope_level,
            node_type
        ));
    }

    fn visit_assign(&mut self, node: &NodeType<'a>) {
        let children = node.children();
        self.visit(children[0]);
        self.output.push_str(" := ");
        self.visit(children[1]);
        self.output.push('\n');
    }

    fn visit_var(&mut self, node: &NodeType<'a>) {
        if self.scopes.lookup(node.value().unwrap()).is_none() {
            panic!("Not in SymbolTable: {}", node.value().unwrap())
        }

        let symbol_tuple = self.scopes.lookup(node.value().unwrap()).unwrap();
        let symbol = match symbol_tuple.0 {
            Symbol::Var(var) => var,
            _ => panic!("Invalid symbol: {:?}", symbol_tuple.0),
        };
        self.output.push_str(&format!(
            "<{}{}:{}>",
            symbol.name,
            symbol_tuple.1,
            match symbol.kind {
                KindSymbol::Integer => "INTEGER",
                KindSymbol::Real => "REAL",
            }
        ));
    }

    fn symbol_type(&self, symbol: &Symbol) -> KindSymbol {
        match symbol {
            Symbol::Kind(matched_symbol) => matched_symbol.clone(),
            _ => panic!("No support for this Symbol: {:?}", symbol),
        }
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
            Node::Program => self.visit_program(node),
            Node::Block => self.visit_block(node),
            Node::VarDecl => self.visit_var_decl(),
            Node::Type => self.visit_type(),
            Node::Procedure => self.visit_procedure_decl(),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        };
    }

    fn visit_expr(&self, node: &NodeType<'a>) -> f32 {
        match node.node() {
            Node::Num => self.visit_num(node),
            Node::BinOp => self.visit_bin_op(node),
            Node::UnaryOp => self.visit_unary_op(node),
            Node::Var => self.visit_var(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        }
    }

    fn visit_program(&mut self, node: &NodeType<'a>) {
        self.visit(node.children()[0])
    }

    fn visit_block(&mut self, node: &NodeType<'a>) {
        node.children()
            .into_iter()
            .for_each(|node_elt| self.visit(node_elt));
    }

    fn visit_var_decl(&mut self) {}

    fn visit_type(&mut self) {}

    fn visit_bin_op(&self, node: &NodeType<'a>) -> f32 {
        let children = node.children();
        match node.token().unwrap().kind {
            TokenKind::Plus => self.visit_expr(children[0]) + self.visit_expr(children[1]),
            TokenKind::Minus => self.visit_expr(children[0]) - self.visit_expr(children[1]),
            TokenKind::Mul => self.visit_expr(children[0]) * self.visit_expr(children[1]),
            TokenKind::IntegerDiv => {
                let left_children = self.visit_expr(children[0]);
                let right_children = self.visit_expr(children[1]);
                if left_children.fract() != 0_f32 || right_children.fract() != 0_f32 {
                    panic!(
                        "Not an integer. Left: {}. Right: {}",
                        left_children, right_children
                    );
                }

                left_children / right_children
            }
            TokenKind::FloatDiv => self.visit_expr(children[0]) / self.visit_expr(children[1]),
            _ => panic!("Invalid op. Op: {:?}", node.token()),
        }
    }

    fn visit_num(&self, node: &NodeType<'a>) -> f32 {
        node.value().unwrap().parse::<f32>().unwrap()
    }

    fn visit_unary_op(&self, node: &NodeType<'a>) -> f32 {
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

    fn visit_var(&self, node: &NodeType<'a>) -> f32 {
        match self.global_scope.get(node.value().unwrap()) {
            Some(var_value) => var_value.parse::<f32>().unwrap(),
            None => panic!("No Value for var {:?}", node.token()),
        }
    }

    fn visit_no_op(&self) {}

    fn visit_procedure_decl(&mut self) {}

    pub fn interpret(&mut self, tree: &NodeType<'a>) {
        self.visit(tree)
    }
}
