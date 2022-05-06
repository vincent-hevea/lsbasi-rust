use crate::error::{Error, ErrorCategory, ErrorKind};
use crate::parser::{KindSymbol, Node, NodeType, ProcedureSymbol, Symbol, VarSymbol};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
struct ScopedSymbolTable<'a> {
    symbols: HashMap<String, Symbol<'a>>,
    scope_name: String,
    scope_level: i32,
}

impl<'a> ScopedSymbolTable<'a> {
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

    fn insert(&mut self, symbol: Symbol<'a>) {
        match symbol {
            Symbol::Kind(matched_symbol) => match matched_symbol {
                KindSymbol::Integer => self
                    .symbols
                    .insert("INTEGER".to_string(), Symbol::Kind(KindSymbol::Integer)),
                KindSymbol::Real => self
                    .symbols
                    .insert("REAL".to_string(), Symbol::Kind(KindSymbol::Real)),
            },
            Symbol::Var(matched_symbol) => self.symbols.insert(
                matched_symbol.name().to_string(),
                Symbol::Var(matched_symbol),
            ),
            Symbol::Procedure(matched_symbol) => self.symbols.insert(
                matched_symbol.name().to_string(),
                Symbol::Procedure(matched_symbol),
            ),
        };
    }

    fn lookup(&self, name: &str) -> Option<&Symbol<'a>> {
        self.symbols.get(name)
    }
}

#[derive(Debug)]
pub struct ScopedSymbolTableStack<'a> {
    stack: Vec<ScopedSymbolTable<'a>>,
}

impl<'a> ScopedSymbolTableStack<'a> {
    fn new() -> Self {
        ScopedSymbolTableStack { stack: Vec::new() }
    }

    fn lookup(&self, name: &str) -> Option<(&Symbol<'a>, i32)> {
        for scope in self.stack.iter().rev() {
            if let Some(symbol) = scope.lookup(name) {
                return Some((symbol, scope.scope_level));
            }
        }

        None
    }

    fn lookup_current_scope(&self, name: &str) -> Option<&Symbol<'a>> {
        self.stack.last().and_then(|scope| scope.lookup(name))
    }
}

pub struct SemanticAnalyzer<'a> {
    pub scopes: ScopedSymbolTableStack<'a>,
}

impl<'a> Default for SemanticAnalyzer<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> SemanticAnalyzer<'a> {
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
            Node::ProcedureCall => self.visit_procedure_call(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        };
    }

    fn visit_program(&mut self, node: &NodeType<'a>) {
        self.scopes
            .stack
            .push(ScopedSymbolTable::new("global".to_string(), 1));
        println!("visit_program {:#?}", self.scopes);
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
            .lookup_current_scope(children[0].token().unwrap().value().as_ref())
            .is_some()
        {
            panic!(
                "{}",
                Error::new(
                    format!(
                        "Duplicate identifier: {}",
                        children[0].token().unwrap().value()
                    ),
                    ErrorKind::DuplicateId,
                    children[0].token().unwrap().line(),
                    children[0].token().unwrap().col(),
                    children[0].token().unwrap().value().to_string(),
                    ErrorCategory::Semantic
                )
            );
        }

        self.scopes
            .stack
            .last_mut()
            .unwrap()
            .insert(Symbol::Var(VarSymbol::new(symbol_name, symbol_type)));
        println!("visit_var_decl {:#?}", self.scopes);
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
            panic!(
                "{}",
                Error::new(
                    format!("Not in SymbolTable: {}", node.value().unwrap()),
                    ErrorKind::IdNotFound,
                    node.token().unwrap().line(),
                    node.token().unwrap().col(),
                    node.token().unwrap().value().to_string(),
                    ErrorCategory::Semantic
                )
            );
        }
    }

    fn visit_no_op(&self) {}

    fn visit_procedure_decl(&mut self, node: &NodeType<'a>) {
        let mut procedure_symbol = ProcedureSymbol::new(
            String::from(node.value().unwrap()),
            Vec::new(),
            Rc::clone(node.block().unwrap()),
        );
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
                procedure_symbol.push_params(VarSymbol::new(
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
        println!("visit_procedure_decl (push) {:#?}", self.scopes);
        self.visit(*node.children().last().unwrap());
        self.scopes.stack.pop();
        println!("visit_procedure_decl (pop) {:#?}", self.scopes);
    }

    fn visit_procedure_call(&mut self, node: &NodeType<'a>) {
        let symbol_info = self.scopes.lookup(node.value().unwrap());
        if symbol_info.is_none() {
            panic!(
                "{}",
                Error::new(
                    format!("Not in SymbolTable: {}", node.value().unwrap()),
                    ErrorKind::IdNotFound,
                    node.token().unwrap().line(),
                    node.token().unwrap().col(),
                    node.value().unwrap().to_string(),
                    ErrorCategory::Semantic
                )
            );
        }

        if let Some(symbol) = symbol_info {
            match symbol.0 {
                Symbol::Procedure(procedure_symbol) => {
                    if procedure_symbol.params().len() != node.children().len() {
                        panic!(
                            "{}",
                            Error::new(
                                "Wrong params num".to_string(),
                                ErrorKind::WrongParamsNum,
                                node.token().unwrap().line(),
                                node.token().unwrap().col(),
                                node.value().unwrap().to_string(),
                                ErrorCategory::Semantic
                            )
                        );
                    }
                    node.set_sd_linked_block(procedure_symbol.block());
                    node.set_sd_signature(procedure_symbol.params().to_vec());
                    node.set_sd_scope_level(symbol.1);
                }
                _ => panic!(
                    "{}",
                    Error::new(
                        "Wrong symbol type".to_string(),
                        ErrorKind::WrongSymbolType,
                        node.token().unwrap().line(),
                        node.token().unwrap().col(),
                        node.value().unwrap().to_string(),
                        ErrorCategory::Semantic
                    )
                ),
            }
        }
        for child in node.children() {
            self.visit(child);
        }
    }

    fn symbol_type(&self, symbol: &Symbol) -> KindSymbol {
        match symbol {
            Symbol::Kind(matched_symbol) => matched_symbol.clone(),
            _ => panic!("No support for this Symbol: {:?}", symbol),
        }
    }
}
