mod ast;
mod callable;
mod class;
mod environment;
mod instance;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod shared_list;
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
    let (tokens, scan_success) = scanner::scan_tokens(source);
    let (mut statements, parse_err) = parser::parse(&tokens);
    if !scan_success || parse_err.is_some() {
        return (false, true);
    }

    let mut resolver = resolver::Resolver::new();
    let mut out = io::stdout();
    match resolver.resolve(&mut statements) {
        Ok(_) => {
            let mut interpreter = interpreter::Interpreter::new(&mut out);
            match interpreter.interpret(&statements) {
                Ok(_) => (scan_success, true),
                Err(err) => {
                    eprintln!("{}", err);
                    (scan_success, false)
                }
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            (false, true)
        }
    }
}
