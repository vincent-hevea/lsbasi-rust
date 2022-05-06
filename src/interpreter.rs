use crate::lexer::TokenKind;
use crate::parser::{Node, NodeType};
use std::collections::HashMap;
use std::iter::zip;

#[derive(Debug)]
enum ArKind {
    Program,
    Procedure,
}

#[derive(Debug)]
struct ActivationRecord {
    name: String,
    kind: ArKind,
    nesting_level: i32,
    members: HashMap<String, String>,
}

impl ActivationRecord {
    fn new(name: String, kind: ArKind, nesting_level: i32) -> Self {
        ActivationRecord {
            name,
            kind,
            nesting_level,
            members: HashMap::new(),
        }
    }

    fn get(&self, key: &str) -> Option<&String> {
        self.members.get(key)
    }

    fn insert(&mut self, key: String, value: String) {
        self.members.insert(key, value);
    }
}

#[derive(Debug)]
struct CallStack {
    records: Vec<ActivationRecord>,
}

impl CallStack {
    fn new() -> Self {
        CallStack {
            records: Vec::new(),
        }
    }

    fn push(&mut self, ar: ActivationRecord) {
        self.records.push(ar);
    }

    fn pop(&mut self) -> Option<ActivationRecord> {
        self.records.pop()
    }

    fn peek_mut(&mut self) -> Option<&mut ActivationRecord> {
        self.records.last_mut()
    }
}

pub struct Interpreter {
    call_stack: CallStack,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        Interpreter {
            call_stack: CallStack::new(),
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
            Node::ProcedureCall => self.visit_procedure_call(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        };
    }

    fn visit_expr(&mut self, node: &NodeType<'a>) -> f32 {
        match node.node() {
            Node::Num => self.visit_num(node),
            Node::BinOp => self.visit_bin_op(node),
            Node::UnaryOp => self.visit_unary_op(node),
            Node::Var => self.visit_var(node),
            _ => panic!("Invalid node. Node: {:?}", node.node()),
        }
    }

    fn visit_program(&mut self, node: &NodeType<'a>) {
        println!("visit_program (start) {:?}", self.call_stack);
        self.call_stack.push(ActivationRecord::new(
            node.value().unwrap().to_string(),
            ArKind::Program,
            1,
        ));
        println!("visit_program (first ar) {:#?}", self.call_stack);
        self.visit(node.children()[0]);
        self.call_stack.pop();
        println!("visit_program (end) {:#?}", self.call_stack);
    }

    fn visit_block(&mut self, node: &NodeType<'a>) {
        node.children()
            .into_iter()
            .for_each(|node_elt| self.visit(node_elt));
    }

    fn visit_var_decl(&mut self) {}

    fn visit_type(&mut self) {}

    fn visit_bin_op(&mut self, node: &NodeType<'a>) -> f32 {
        let children = node.children();
        match node.token().unwrap().kind() {
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

    fn visit_unary_op(&mut self, node: &NodeType<'a>) -> f32 {
        match node.token().unwrap().kind() {
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
        let value = self.visit_expr(children[1]).to_string();
        self.call_stack
            .peek_mut()
            .unwrap()
            .insert(children[0].token().unwrap().value().to_string(), value);
        println!("visit_assign {:#?}", self.call_stack);
    }

    fn visit_var(&mut self, node: &NodeType<'a>) -> f32 {
        match self
            .call_stack
            .peek_mut()
            .unwrap()
            .get(node.value().unwrap())
        {
            Some(var_value) => var_value.parse::<f32>().unwrap(),
            None => panic!("No Value for var {:?}", node.token()),
        }
    }

    fn visit_no_op(&self) {}

    fn visit_procedure_decl(&mut self) {}

    fn visit_procedure_call(&mut self, node: &NodeType<'a>) {
        let mut ar = ActivationRecord::new(
            node.value().unwrap().to_string(),
            ArKind::Procedure,
            *node.sd_scope_level().unwrap().borrow() + 1,
        );

        for (param, arg) in zip(&*node.sd_signature().unwrap().borrow(), node.children()) {
            ar.insert(param.name().to_string(), self.visit_expr(arg).to_string());
        }

        self.call_stack.push(ar);
        println!("visit_procedure_call (push ar) {:#?}", self.call_stack);
        self.visit(&**node.sd_linked_block().unwrap().borrow().upgrade().unwrap());

        self.call_stack.pop();
        println!("visit_procedure_call (pop ar) {:#?}", self.call_stack);
    }

    pub fn interpret(&mut self, tree: &NodeType<'a>) {
        self.visit(tree)
    }
}
