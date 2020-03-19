use clap::{App, Arg};
use std::fs;
use std::io::{self, Write};

use rlox::vm::compiler;
use rlox::vm::vm;

fn main() {
    let matches = App::new("My Super Program")
        .arg(
            Arg::with_name("trace")
                .long("trace")
                .help("Trace execution"),
        )
        .arg(Arg::with_name("INPUT").help("Lox script to execute"))
        .get_matches();
    let trace = matches.is_present("trace");
    if matches.is_present("INPUT") {
        run_file(matches.value_of("INPUT").unwrap(), trace);
    } else {
        run_prompt(trace);
    }
}

fn run_file(file: &str, trace: bool) {
    let contents = fs::read_to_string(file).expect("Something went wrong reading the file");
    let result = interpret(&contents, trace);
    match result {
        compiler::InterpretResult::CompileError => std::process::exit(65),
        compiler::InterpretResult::RuntimeError => std::process::exit(70),
        _ => (),
    }
}

fn run_prompt(trace: bool) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        interpret(&line, trace);
    }
}

fn interpret(source: &str, trace: bool) -> compiler::InterpretResult {
    match compiler::compile(source, trace) {
        Ok(chunk) => vm::interpret(&chunk, trace),
        Err(e) => e,
    }
}
