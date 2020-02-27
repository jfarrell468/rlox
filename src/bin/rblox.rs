use num_enum::TryFromPrimitive;
use rlox::scanner;
use rlox::token::{Token, TokenType};
use std::convert::{TryFrom, TryInto};
use std::env;
use std::fmt;
use std::fmt::Formatter;
use std::fs;
use std::io::{self, Write};
use strum_macros::Display;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Display)]
#[repr(u8)]
enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Return,
}

impl OpCode {
    pub fn simple_instruction(&self, offset: usize) -> usize {
        println!("{}", self);
        offset + 1
    }
    pub fn constant_instruction(&self, chunk: &Chunk, offset: usize) -> usize {
        let constant = chunk.code[offset + 1] as usize;
        println!(
            "{:16} {:4} '{}'",
            format!("{}", self),
            constant,
            chunk.constant[constant]
        );
        offset + 2
    }
}

#[derive(Clone, Debug)]
enum Value {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String)
}

impl<'a> fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Number(x) => {
                if x.is_sign_negative() {
                    write!(f, "-{}", -x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Value::String(x) => write!(f, "{}", x),
        }
    }
}

impl Value {
    fn is_falsey(&self) -> bool {
        match self {
            Value::Boolean(x) => !*x,
            Value::Nil => true,
            Value::Number(_) => false,
            Value::String(_) => false
        }
    }
    fn equals(&self, other: &Value) -> bool {
        match self {
            Value::Boolean(a) => match other {
                Value::Boolean(b) => a == b,
                _ => false,
            },
            Value::Nil => match other {
                Value::Nil => true,
                _ => false,
            },
            Value::Number(a) => match other {
                Value::Number(b) => a == b,
                _ => false,
            },
            Value::String(a) => match other {
                Value::String(b) => a == b,
                _ => false,
            },
        }
    }
}

struct Chunk {
    pub code: Vec<u8>,
    pub constant: Vec<Value>,
    pub line: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constant: Vec::new(),
            line: Vec::new(),
        }
    }
    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.line.push(line);
    }
    pub fn add_constant(&mut self, value: Value) -> Result<u8, std::num::TryFromIntError> {
        let result: Result<u8, std::num::TryFromIntError> = self.constant.len().try_into();
        self.constant.push(value);
        result
    }
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset: usize = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.line[offset] == self.line[offset.checked_sub(1).unwrap_or(0)] {
            print!("   | ");
        } else {
            print!("{:4} ", self.line[offset]);
        }
        let instruction = OpCode::try_from(self.code[offset]).unwrap();
        match instruction {
            OpCode::Constant => instruction.constant_instruction(self, offset),
            _ => instruction.simple_instruction(offset),
        }
    }
}

struct Vm<'a> {
    pub chunk: Option<&'a Chunk>,
    pub ip: Option<std::iter::Peekable<std::iter::Enumerate<std::slice::Iter<'a, u8>>>>,
    pub stack: Vec<Value>,
    pub trace_execution: bool,
}

enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

macro_rules! binary_op {
    ($self:expr, $valtype:tt, $op:tt) => {
        if let Value::Number(b) = $self.peek(0) {
            if let Value::Number(a) = $self.peek(1) {
                $self.stack.pop().unwrap();
                $self.stack.pop().unwrap();
                $self.stack.push(Value::$valtype(a $op b));
            } else {
                $self.runtime_error("Operands must be numbers.");
                return InterpretResult::RuntimeError;
            }
        } else {
            $self.runtime_error("Operands must be numbers.");
            return InterpretResult::RuntimeError;
        }
    }
}

impl<'a> Vm<'a> {
    pub fn interpret(&mut self, chunk: &'a Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
        self.ip = Some(chunk.code.iter().enumerate().peekable());
        self.run()
    }
    fn run(&mut self) -> InterpretResult {
        loop {
            if self.trace_execution {
                print!("          ");
                for val in &self.stack {
                    print!("[ {} ]", val);
                }
                println!("");
                self.chunk.unwrap().disassemble_instruction(
                    self.ip.as_mut().map(|x| x.peek()).unwrap().unwrap().0,
                );
            }
            let instruction = OpCode::try_from(self.read_byte()).unwrap();
            match instruction {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant.clone());
                }
                OpCode::Nil => {
                    self.stack.push(Value::Nil);
                }
                OpCode::True => {
                    self.stack.push(Value::Boolean(true));
                }
                OpCode::False => {
                    self.stack.push(Value::Boolean(false));
                }
                OpCode::Equal => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a.equals(&b)));
                }
                OpCode::Greater => {
                    binary_op!(self, Boolean, >);
                }
                OpCode::Less => {
                    binary_op!(self, Boolean, <);
                }
                OpCode::Add => {
                    let bv = self.peek(0);
                    let av = self.peek(1);
                    match av {
                        Value::String(mut a) => match bv {
                            Value::String(b) => {
                                self.stack.pop();
                                self.stack.pop();
                                a.push_str(b.as_str());
                                self.stack.push(Value::String(a));
                            },
                            _ => {
                                self.runtime_error("Operands must be two numbers or two strings.");
                                return InterpretResult::RuntimeError;
                            }
                        },
                        Value::Number(a) => match bv {
                            Value::Number(b) => {
                                self.stack.pop();
                                self.stack.pop();
                                self.stack.push(Value::Number(a+b));
                            },
                            _ => {
                                self.runtime_error("Operands must be two numbers or two strings.");
                                return InterpretResult::RuntimeError;
                            }
                        },
                        _ => {
                            self.runtime_error("Operands must be two numbers or two strings.");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                OpCode::Subtract => {
                    binary_op!(self, Number, -);
                }
                OpCode::Multiply => {
                    binary_op!(self, Number, *);
                }
                OpCode::Divide => {
                    binary_op!(self, Number, /);
                }
                OpCode::Not => {
                    let val = self.stack.pop().unwrap().is_falsey();
                    self.stack.push(Value::Boolean(val));
                }
                OpCode::Negate => match self.peek(0) {
                    Value::Number(x) => {
                        self.stack.pop().unwrap();
                        self.stack.push(Value::Number(-x));
                    }
                    _ => {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                },
                OpCode::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    return InterpretResult::Ok;
                }
            }
        }
    }
    fn read_byte(&mut self) -> u8 {
        *self.ip.as_mut().map(|x| x.next()).unwrap().unwrap().1
    }
    fn read_constant(&mut self) -> Value {
        self.chunk
            .unwrap()
            .constant
            .get(self.read_byte() as usize)
            .unwrap()
            .clone()
    }
    fn peek(&self, distance: usize) -> Value {
        self.stack
            .get(self.stack.len() - 1 - distance)
            .unwrap()
            .clone()
    }
    fn current_line(&mut self) -> usize {
        self.ip.as_mut().map_or(0, |x| x.peek().map_or(0, |x| x.0))
    }
    fn runtime_error(&mut self, msg: &str) {
        eprintln!("{}", msg);
        eprintln!("[line {}] in script", self.current_line());
        self.reset_stack();
    }
    fn reset_stack(&mut self) {
        self.stack.clear();
    }
}

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
    let result = interpret(&contents);
    match result {
        InterpretResult::CompileError => std::process::exit(65),
        InterpretResult::RuntimeError => std::process::exit(70),
        _ => (),
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
        interpret(&line);
    }
}

fn interpret(source: &str) -> InterpretResult {
    let mut parser = Parser::new();
    match parser.compile(source) {
        Ok(chunk) => {
            let mut vm = Vm {
                chunk: None,
                ip: None,
                stack: Vec::new(),
                trace_execution: false,
            };
            vm.interpret(&chunk);
        }
        Err(e) => {
            return e;
        }
    }
    InterpretResult::Ok
}

struct Parser<'a> {
    chunk: Option<Chunk>,
    scanner: scanner::Scanner<'a>,
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    error: bool,
    panic_mode: bool,
}

macro_rules! consume {
    ($self:expr, $token_type:pat, $error:expr) => {
        match $self.current.as_ref().unwrap().tokentype {
            $token_type => $self.advance(),
            _ => $self.error_at_current($error),
        }
    };
}

#[derive(Debug, TryFromPrimitive, Display, PartialEq, PartialOrd, Clone, Copy)]
#[repr(u8)]
enum ParsePrecedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

struct ParseRule {
    prefix: Option<fn(&mut Parser) -> ()>,
    infix: Option<fn(&mut Parser) -> ()>,
    precedence: ParsePrecedence,
}

impl<'a> ParseRule {
    fn get_rule(tt: TokenType) -> &'static ParseRule {
        &RULES[(tt as u8) as usize]
    }
}

macro_rules! lambdafy {
    (None) => {
        None
    };
    ($method:ident) => {
        Some(|p: &mut Parser| p.$method())
    };
}

macro_rules! mkrules {
    ($($prefix:tt, $infix:tt, $precedence:tt) ; +) => {
        &[
        $(
            ParseRule {
                prefix: lambdafy!($prefix),
                infix: lambdafy!($infix),
                precedence: ParsePrecedence::$precedence
            }
        ),+
        ]
    };
}

#[rustfmt::skip]
static RULES : &[ParseRule] = mkrules!(
    grouping, None,    None;       // LeftParen
    None,     None,    None;       // RightParen
    None,     None,    None;       // LeftBrance
    None,     None,    None;       // RightBrace
    None,     None,    None;       // Comma
    None,     None,    None;       // Dot
    unary,    binary,  Term;       // Minus
    None,     binary,  Term;       // Plus
    None,     None,    None;       // Semicolon
    None,     binary,  Factor;     // Slash
    None,     binary,  Factor;     // Star
    unary,    None,    None;       // Bang
    None,     binary,  Equality;   // BangEqual
    None,     None,    None;       // Equal
    None,     binary,  Equality;   // EqualEqual
    None,     binary,  Comparison; // Greater
    None,     binary,  Comparison; // GreaterEqual
    None,     binary,  Comparison; // Less
    None,     binary,  Comparison; // LessEqual
    None,     None,    None;       // Identifier
    string,   None,    None;       // String
    number,   None,    None;       // Number
    None,     None,    None;       // And
    None,     None,    None;       // Class
    None,     None,    None;       // Else
    literal,  None,    None;       // False
    None,     None,    None;       // For
    None,     None,    None;       // Fun
    None,     None,    None;       // If
    literal,  None,    None;       // Nil
    None,     None,    None;       // Or
    None,     None,    None;       // Print
    None,     None,    None;       // Return
    None,     None,    None;       // Super
    None,     None,    None;       // This
    literal,  None,    None;       // True
    None,     None,    None;       // Var
    None,     None,    None;       // While
    None,     None,    None        // EOF
);

impl<'a> Parser<'a> {
    fn new() -> Parser<'a> {
        Parser {
            chunk: None,
            scanner: scanner::Scanner::new(""),
            current: None,
            previous: None,
            error: false,
            panic_mode: false,
        }
    }
    fn print_error(token: &Token<'a>, message: &str) {
        eprint!("[line {}] Error", token.line);

        if let TokenType::EOF = token.tokentype {
            eprint!(" at end");
        } else {
            eprint!(" at '{}'", token.lexeme);
        }

        eprintln!(": {}", message);
    }
    fn error_at_current(&mut self, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.error = true;
        Parser::print_error(self.current.as_ref().unwrap(), message)
    }
    fn error(&mut self, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.error = true;
        Parser::print_error(self.previous.as_ref().unwrap(), message)
    }
    fn advance(&mut self) {
        self.previous = self.current.take();

        loop {
            match self.scanner.scan_token_for_rblox() {
                Ok(token) => {
                    self.current = Some(token);
                    break;
                }
                Err(e) => {
                    self.error_at_current(format!("{}", e).as_str());
                }
            }
        }
    }
    fn compile(&mut self, source: &'a str) -> Result<Chunk, InterpretResult> {
        self.chunk = Some(Chunk::new());
        self.scanner = scanner::Scanner::new(source);
        self.advance();
        self.expression();
        consume!(self, TokenType::EOF, "Expect end of expression.");
        self.emit_byte(OpCode::Return as u8);
        if self.error {
            self.chunk.as_ref().map(|c| c.disassemble("code"));
            Err(InterpretResult::CompileError)
        } else {
            Ok(self.chunk.take().unwrap())
        }
    }
    fn emit_byte(&mut self, byte: u8) {
        self.chunk
            .as_mut()
            .unwrap()
            .write(byte, self.previous.as_ref().unwrap().line as usize);
    }
    fn emit_bytes(&mut self, byte: u8, byte2: u8) {
        self.emit_byte(byte);
        self.emit_byte(byte2);
    }
    fn expression(&mut self) {
        self.parse_precedence(ParsePrecedence::Assignment)
    }
    fn number(&mut self) {
        let value: f64 = self.previous.as_ref().unwrap().lexeme.parse().unwrap();
        self.emit_constant(Value::Number(value));
    }
    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, constant);
    }
    fn make_constant(&mut self, value: Value) -> u8 {
        match self.chunk.as_mut().unwrap().add_constant(value) {
            Ok(c) => c,
            Err(_) => {
                self.error("Too many constants in one chunk");
                0
            }
        }
    }
    fn grouping(&mut self) {
        self.expression();
        consume!(self, TokenType::RightParen, "Expect ')' after expression.");
    }
    fn unary(&mut self) {
        let prev = self.previous.take().unwrap();
        self.parse_precedence(ParsePrecedence::Unary);
        match prev.tokentype {
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            _ => (),
        }
    }
    fn binary(&mut self) {
        let prev = self.previous.take().unwrap();
        let rule = ParseRule::get_rule(prev.tokentype);
        self.parse_precedence(ParsePrecedence::try_from(rule.precedence as u8 + 1).unwrap());
        match prev.tokentype {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => (),
        }
    }
    fn literal(&mut self) {
        match self.previous.as_ref().unwrap().tokentype {
            TokenType::False => self.emit_byte(OpCode::False as u8),
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            _ => (),
        }
    }
    fn string(&mut self) {
        let lexeme = self.previous.as_ref().unwrap().lexeme;
        self.emit_constant(Value::String(lexeme[1..lexeme.len()-1].to_string()));
    }
    fn parse_precedence(&mut self, prec: ParsePrecedence) {
        self.advance();
        let prefix_rule = ParseRule::get_rule(self.previous.as_ref().unwrap().tokentype).prefix;
        match prefix_rule {
            None => self.error("Expect expression"),
            Some(f) => {
                f(self);

                while prec
                    <= ParseRule::get_rule(self.current.as_ref().unwrap().tokentype).precedence
                {
                    self.advance();
                    let infix_rule =
                        ParseRule::get_rule(self.previous.as_ref().unwrap().tokentype).infix;
                    infix_rule.map(|f| f(self));
                }
            }
        }
    }
}
