use lsbasi_rust::{Interpreter, Lexer, Parser, SemanticAnalyzer, SourceToSourceCompiler};
use std::fs;

fn main() {
    let input = fs::read_to_string("prg16.pas").expect("Something went wrong reading the file");

    let mut lexer = Lexer::new(&input);
    let mut parser = Parser::new(&mut lexer);
    let tree = parser.parse();

    // let mut symtab_builder = SemanticAnalyzer::new();
    // symtab_builder.visit(&*tree);

    let mut source_compiler = SourceToSourceCompiler::new();
    source_compiler.visit(&*tree);

    println!("{}", source_compiler.output());

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&*tree);

    println!("{:?}", interpreter.global_scope);
}
