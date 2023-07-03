pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod map;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;

fn main() {
    repl::start();
}
