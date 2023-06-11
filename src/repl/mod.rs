use std::io::{self, BufRead, Write};

use crate::{
    ast::Node, evaluator::eval, lexer::Lexer, object::environ::Environment, parser::Parser,
};

const PROMPT: &str = ">> ";

pub fn start() {
    let stdin = io::stdin();
    let mut environ = Environment::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        // read line
        let mut line = String::new();
        if let Err(err) = stdin.lock().read_line(&mut line) {
            eprintln!("Failed to read line from stdin: {:?}", err);
            break;
        }

        if line.len() == 0 {
            break;
        }

        let l = Lexer::new(&line);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let res = eval(&Node::Program(program), &mut environ);
        println!("{}", res);
    }
}
