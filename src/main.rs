use lsbasi_rust::{Interpreter, Lexer, Parser, SymbolTableBuilder};
use std::fs;

fn main() {
    let input = fs::read_to_string("prg11.pas").expect("Something went wrong reading the file");

    let mut lexer = Lexer::new(&input);
    let mut parser = Parser::new(&mut lexer);
    let tree = parser.parse();

    let mut symtab_builder = SymbolTableBuilder::new();
    symtab_builder.visit(&*tree);

    println!("{:#?}", symtab_builder.sym_tab);

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&*tree);

    println!("{:?}", interpreter.global_scope);
}
