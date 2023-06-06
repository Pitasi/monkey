use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn start() {
    let stdin = io::stdin();

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

        let mut l = Lexer::new(&line);
        // TODO: Lexer could return an iterator
        let mut tok = l.next_token();
        while tok != Token::EOF {
            println!("{:?}", tok);
            tok = l.next_token();
        }
    }
}
