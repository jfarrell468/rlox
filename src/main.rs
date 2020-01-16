mod ast;
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
    if let Err(_) = run(&contents) {
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
        if let Err(_) = run(&line) {
            println!("error")
        }
    }
}

fn run(source: &str) -> Result<(), ()> {
    let mut scanner = scanner::Scanner::from_string(source);
    let tokens = scanner.scan_tokens().0;
    for token in tokens {
        println!("{:?}", token);
    }
    let mut parser = parser::Parser::new(tokens);
    match parser.parse() {
        Ok(expr) => {
            let printer = ast::AstPrinter {};
            println!("{}", expr.accept(&printer));
        }
        Err(x) => {
            println!("{}", x);
        }
    }
    Ok(())
}
