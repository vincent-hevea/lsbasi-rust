use lsbasi_rust::interpreter::Interpreter;
use lsbasi_rust::lexer::Lexer;
use lsbasi_rust::parser::Parser;
use lsbasi_rust::semantic_analyser::SemanticAnalyzer;
use std::fs;

fn main() {
    let input = fs::read_to_string("prg19.pas").expect("Something went wrong reading the file");

    let mut lexer = Lexer::new(&input);
    let mut parser = Parser::new(&mut lexer);
    let tree = parser.parse();

    let mut symtab_builder = SemanticAnalyzer::new();
    symtab_builder.visit(&*tree);

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&*tree);
}
