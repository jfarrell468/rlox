mod ast;
mod callable;
mod environment;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod shared_list;
mod token;

use crate::ast::Value;
use crate::interpreter::ErrorType;
use crate::resolver::ResolverError;
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
    let result = run(&contents);
    if !result.0 {
        std::process::exit(65);
    }
    if !result.1 {
        std::process::exit(70);
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

fn run(source: &str) -> (bool, bool) {
    let (tokens, success) = scanner::scan_tokens(source);
    if !success {
        return (false, true);
    }
    match &mut parser::parse(&tokens) {
        Ok(statements) => {
            let mut resolver = resolver::Resolver::new();
            match resolver.resolve(statements) {
                Ok(_) => {
                    let mut interpreter = interpreter::Interpreter::new();
                    match interpreter.interpret(statements) {
                        Ok(_) => (true, true),
                        Err(err) => {
                            eprintln!("{}", err);
                            (true, false)
                        }
                    }
                }
                Err(err) => {
                    eprintln!("{}", err);
                    (false, true)
                }
            }
        }
        Err(x) => {
            eprintln!("{}", x);
            (false, true)
        }
    }
}
