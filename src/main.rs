mod ast;
mod callable;
mod environment;
mod interpreter;
mod parser;
mod scanner;
mod token;

use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rlox [script]");
            // TODO: https://rust-cli.github.io/book/in-depth/exit-code.html
            std::process::exit(64);
        }
    }
}

fn run_file(file: &str) {
    let contents = fs::read_to_string(file).expect("Something went wrong reading the file");
    if !run(&contents) {
        std::process::exit(65);
    }
}

fn run_prompt() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        run(&line);
    }
}

fn run(source: &str) -> bool {
    let (tokens, success) = scanner::scan_tokens(source);
    if !success {
        return success;
    }
    match parser::parse(&tokens) {
        Ok(statements) => {
            let mut interpreter = interpreter::Interpreter::new();
            interpreter.interpret(&statements);
            true
        }
        Err(x) => {
            println!("{}", x);
            false
        }
    }
}
