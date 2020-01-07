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
        },
    }
}

fn run_file(file: &str) {
    let contents = fs::read_to_string(file)
        .expect("Something went wrong reading the file");
    run(&contents);
}

fn run_prompt() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failed to read line");
        run(&line);
    }
}

fn run(source: &str) {
    let prefix: String = source.trim().chars().take(50).collect();
    println!("run({})", prefix)
}