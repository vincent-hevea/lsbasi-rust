use lsbasi_rust::{Interpreter, Lexer, Parser};
use std::io;
use std::io::Write;

fn main() {
    loop {
        print!("calc> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let input: String = match input.trim().parse() {
            Ok(input_ok) => input_ok,
            Err(_) => continue,
        };

        if input.is_empty() {
            println!("Bye");
            break;
        }

        let mut lexer = Lexer::new(&input);
        let mut parser = Parser::new(&mut lexer);
        let tree = parser.parse();
        let interpreter = Interpreter::new();
        let result = interpreter.interpret(&*tree);

        println!("{}", result);
    }
}
