use crate::error::{Error, ErrorCategory, ErrorKind};
use crate::lexer::{Lexer, Token, TokenKind};
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::rc::{Rc, Weak};

pub type NodeType<'a> = dyn Ast<'a> + 'a;

impl<'a> fmt::Display for NodeType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.node(), self.value(),)
    }
}

impl<'a> Debug for NodeType<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?} {:?}", self.node(), self.value(),)
    }
}

#[derive(PartialEq, Debug)]
pub enum Node {
    BinOp,
    Num,
    UnaryOp,
    Program,
    Procedure,
    ProcedureCall,
    Params,
    Block,
    Compound,
    Assign,
    Var,
    VarDecl,
    Type,
    NoOp,
}

#[derive(Debug, Clone)]
pub enum KindSymbol {
    Integer,
    Real,
}

#[derive(Debug, Clone)]
pub struct VarSymbol {
    name: String,
    kind: KindSymbol,
}

impl VarSymbol {
    pub fn new(name: String, kind: KindSymbol) -> Self {
        VarSymbol { name, kind }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn kind(&self) -> &KindSymbol {
        &self.kind
    }
}

#[derive(Debug)]
pub struct ProcedureSymbol<'a> {
    name: String,
    params: Vec<VarSymbol>,
    block: Rc<Box<NodeType<'a>>>,
}

impl<'a> ProcedureSymbol<'a> {
    pub fn new(name: String, params: Vec<VarSymbol>, block: Rc<Box<NodeType<'a>>>) -> Self {
        ProcedureSymbol {
            name,
            params,
            block,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> &Vec<VarSymbol> {
        &self.params
    }

    pub fn push_params(&mut self, var_symbol: VarSymbol) {
        self.params.push(var_symbol)
    }

    pub fn block(&self) -> &Rc<Box<NodeType<'a>>> {
        &self.block
    }
}

#[derive(Debug)]
pub enum Symbol<'a> {
    Kind(KindSymbol),
    Var(VarSymbol),
    Procedure(ProcedureSymbol<'a>),
}

pub trait Ast<'a> {
    fn node(&self) -> Node;
    fn token(&self) -> Option<&Token>;
    fn value(&self) -> Option<&str>;
    fn children(&self) -> Vec<&NodeType<'a>>;
    fn block(&self) -> Option<&Rc<Box<NodeType<'a>>>> {
        None
    }

    // Semantic decoration (sd)
    // Setter used during Semantic analysis
    // Getter used during Interpretation
    fn sd_linked_block(&self) -> Option<&RefCell<Weak<Box<NodeType<'a>>>>> {
        None
    }
    fn set_sd_linked_block(&self, block: &Rc<Box<NodeType<'a>>>) {}
    fn sd_signature(&self) -> Option<&RefCell<Vec<VarSymbol>>> {
        None
    }
    fn set_sd_signature(&self, signature: Vec<VarSymbol>) {}
    fn sd_scope_level(&self) -> Option<&RefCell<i32>> {
        None
    }
    fn set_sd_scope_level(&self, scope_level: i32) {}
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
    block: Rc<Box<NodeType<'a>>>,
}

struct ProcedureCall<'a> {
    token: Token,
    var_node: Box<NodeType<'a>>,
    args: Vec<Box<NodeType<'a>>>,
    sd_linked_block: RefCell<Weak<Box<NodeType<'a>>>>,
    sd_signature: RefCell<Vec<VarSymbol>>,
    sd_scope_level: RefCell<i32>,
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
        let value = token.value().to_string();
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
        let value = token.value().to_string();
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
        let value = token.value().to_string();
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
            block: Rc::from(block),
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
        children.push(&**self.block);

        children
    }

    fn block(&self) -> Option<&Rc<Box<NodeType<'a>>>> {
        Some(&self.block)
    }
}

impl<'a> ProcedureCall<'a> {
    fn new(token: Token, var_node: Box<NodeType<'a>>, args: Vec<Box<NodeType<'a>>>) -> Self {
        ProcedureCall {
            token,
            var_node,
            args,
            sd_linked_block: RefCell::new(Weak::new()),
            sd_signature: RefCell::new(Vec::new()),
            sd_scope_level: RefCell::new(0),
        }
    }
}

impl<'a> Ast<'a> for ProcedureCall<'a> {
    fn node(&self) -> Node {
        Node::ProcedureCall
    }

    fn token(&self) -> Option<&Token> {
        Some(&self.token)
    }

    fn value(&self) -> Option<&str> {
        Some(self.var_node.value().unwrap())
    }

    fn children(&self) -> Vec<&NodeType<'a>> {
        self.args.iter().map(|n| &**n).collect()
    }

    fn sd_linked_block(&self) -> Option<&RefCell<Weak<Box<NodeType<'a>>>>> {
        Some(&self.sd_linked_block)
    }

    fn set_sd_linked_block(&self, block: &Rc<Box<NodeType<'a>>>) {
        *self.sd_linked_block.borrow_mut() = Rc::downgrade(block);
    }

    fn sd_signature(&self) -> Option<&RefCell<Vec<VarSymbol>>> {
        Some(&self.sd_signature)
    }
    fn set_sd_signature(&self, signature: Vec<VarSymbol>) {
        *self.sd_signature.borrow_mut() = signature;
    }
    fn sd_scope_level(&self) -> Option<&RefCell<i32>> {
        Some(&self.sd_scope_level)
    }
    fn set_sd_scope_level(&self, scope_level: i32) {
        *self.sd_scope_level.borrow_mut() = scope_level;
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
            current_token: Token::new(TokenKind::Root, "".to_string()),
        }
    }

    fn eat(&mut self, token_kind: TokenKind) -> Result<(), String> {
        if *self.current_token.kind() == token_kind {
            self.current_token = self.lexer.get_next_token();
            Ok(())
        } else {
            let error_message = format!(
                "Unable to eat. Expected token kind: {:?}. Actual token kind: {:?}",
                token_kind,
                self.current_token.kind()
            );
            Err(Error::new(
                error_message,
                ErrorKind::UnexpectedToken,
                self.current_token.line(),
                self.current_token.col(),
                self.current_token.value().to_string(),
                ErrorCategory::Parser,
            )
            .to_string())
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
        while *self.current_token.kind() == TokenKind::Var {
            self.eat(TokenKind::Var).unwrap();
            while *self.current_token.kind() == TokenKind::Id {
                declarations.append(&mut self.variable_declaration());
                self.eat(TokenKind::Semi).unwrap();
            }
        }
        while *self.current_token.kind() == TokenKind::Procedure {
            declarations.push(self.procedure_declaration());
        }

        declarations
    }

    fn procedure_declaration(&mut self) -> Box<NodeType<'a>> {
        self.eat(TokenKind::Procedure).unwrap();
        let proc_name = self.current_token.value().to_string();
        self.eat(TokenKind::Id).unwrap();
        let mut params: Vec<Box<NodeType>> = Vec::new();
        if *self.current_token.kind() == TokenKind::Lparen {
            self.eat(TokenKind::Lparen).unwrap();
            params = self.formal_parameter_list();
            self.eat(TokenKind::Rparen).unwrap();
        }
        self.eat(TokenKind::Semi).unwrap();
        let block_node = self.block();
        let proc_decl = Box::new(ProcedureDecl::new(proc_name, params, block_node));
        self.eat(TokenKind::Semi).unwrap();

        proc_decl
    }

    fn formal_parameters(&mut self) -> Vec<Box<NodeType<'a>>> {
        let mut tokens = vec![self.current_token.clone()];
        self.eat(TokenKind::Id).unwrap();

        while *self.current_token.kind() == TokenKind::Comma {
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
        if *self.current_token.kind() != TokenKind::Id {
            return Vec::new();
        }

        let mut param_nodes = self.formal_parameters();
        while *self.current_token.kind() == TokenKind::Semi {
            self.eat(TokenKind::Semi).unwrap();
            param_nodes.append(&mut self.formal_parameters());
        }

        param_nodes
    }

    fn variable_declaration(&mut self) -> Vec<Box<NodeType<'a>>> {
        let mut tokens = vec![self.current_token.clone()];
        self.eat(TokenKind::Id).unwrap();

        while *self.current_token.kind() == TokenKind::Comma {
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
        if *self.current_token.kind() == TokenKind::Integer {
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

        while *self.current_token.kind() == TokenKind::Semi {
            self.eat(TokenKind::Semi).unwrap();
            result.push(self.statement());
        }

        result
    }

    fn statement(&mut self) -> Box<NodeType<'a>> {
        match self.current_token.kind() {
            TokenKind::Begin => self.compound_statement(),
            TokenKind::Id => self.id_statment(),
            _ => self.empty(),
        }
    }

    fn id_statment(&mut self) -> Box<NodeType<'a>> {
        let left = self.variable();
        match self.current_token.kind() {
            TokenKind::Lparen => {
                let mut proc_args: Vec<Box<NodeType<'a>>> = Vec::new();
                self.eat(TokenKind::Lparen).unwrap();
                if *self.current_token.kind() != TokenKind::Rparen {
                    proc_args.push(self.expr());
                }
                while *self.current_token.kind() == TokenKind::Comma {
                    self.eat(TokenKind::Comma).unwrap();
                    proc_args.push(self.expr());
                }
                self.eat(TokenKind::Rparen).unwrap();

                Box::new(ProcedureCall::new(
                    left.token().unwrap().clone(),
                    left,
                    proc_args,
                ))
            }
            TokenKind::Assign => {
                let token = self.current_token.clone();
                self.eat(TokenKind::Assign).unwrap();

                Box::new(Assign::new(left, token, self.expr()))
            }
            _ => panic!(
                "{}",
                Error::new(
                    format!("Invalid token. Token kind: {:?}", self.current_token.kind()),
                    ErrorKind::UnexpectedToken,
                    self.current_token.line(),
                    self.current_token.col(),
                    self.current_token.value().to_string(),
                    ErrorCategory::Parser
                )
            ),
        }
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

        match self.current_token.kind() {
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
            _ => panic!(
                "{}",
                Error::new(
                    format!("Invalid token. Token kind: {:?}", self.current_token.kind()),
                    ErrorKind::UnexpectedToken,
                    self.current_token.line(),
                    self.current_token.col(),
                    self.current_token.value().to_string(),
                    ErrorCategory::Parser
                )
            ),
        }
    }

    fn term(&mut self) -> Box<NodeType<'a>> {
        let mut node = self.factor();
        let mut current_token = self.current_token.clone();

        while [TokenKind::Mul, TokenKind::IntegerDiv, TokenKind::FloatDiv]
            .contains(self.current_token.kind())
        {
            match current_token.kind() {
                TokenKind::Mul => self.eat(TokenKind::Mul).unwrap(),
                TokenKind::IntegerDiv => self.eat(TokenKind::IntegerDiv).unwrap(),
                TokenKind::FloatDiv => self.eat(TokenKind::FloatDiv).unwrap(),
                _ => panic!(
                    "{}",
                    Error::new(
                        format!("Invalid token. Token kind: {:?}", self.current_token.kind()),
                        ErrorKind::UnexpectedToken,
                        self.current_token.line(),
                        self.current_token.col(),
                        self.current_token.value().to_string(),
                        ErrorCategory::Parser
                    )
                ),
            }

            node = Box::new(BinOp::new(node, current_token, self.factor()));
            current_token = self.current_token.clone();
        }

        node
    }

    fn expr(&mut self) -> Box<NodeType<'a>> {
        let mut node = self.term();
        let mut current_token = self.current_token.clone();

        while [TokenKind::Plus, TokenKind::Minus].contains(self.current_token.kind()) {
            if *self.current_token.kind() == TokenKind::Plus {
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
        if *self.current_token.kind() != TokenKind::Eof {
            panic!(
                "{}",
                Error::new(
                    format!("Invalid token. Token kind: {:?}", self.current_token.kind()),
                    ErrorKind::UnexpectedToken,
                    self.current_token.line(),
                    self.current_token.col(),
                    self.current_token.value().to_string(),
                    ErrorCategory::Parser
                )
            );
        }

        node
    }
}
