use clap::{App, Arg};
use num_enum::TryFromPrimitive;
use rlox::scanner;
use rlox::token::{Token, TokenType};
use std::collections::BTreeMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fmt::Formatter;
use std::fs;
use std::io::{self, Write};
use std::ops::Sub;
use strum_macros::Display;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Display)]
#[repr(u8)]
enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
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
            "{:16} {:4} '{:?}'",
            format!("{}", self),
            constant,
            chunk.constant[constant]
        );
        offset + 2
    }
    pub fn byte_instruction(&self, chunk: &Chunk, offset: usize) -> usize {
        let slot = chunk.code[offset + 1];
        println!("{:16} {:4}", format!("{}", self), slot);
        offset + 2
    }
    pub fn jump_instruction(&self, sign: i32, chunk: &Chunk, offset: usize) -> usize {
        let mut jump = (chunk.code[offset + 1] as u16) << 8;
        jump |= chunk.code[offset + 2] as u16;
        println!(
            "{:16} {:4} -> {}",
            format!("{}", self),
            offset,
            (offset as i32) + 3 + sign * (jump as i32)
        );
        offset + 3
    }
}

struct Function {
    arity: usize,
    chunk: Chunk,
    name: Option<String>,
}

#[derive(Clone, Debug)]
enum Value {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
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
            Value::String(_) => false,
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
            OpCode::Constant | OpCode::DefineGlobal | OpCode::SetGlobal | OpCode::GetGlobal => {
                instruction.constant_instruction(self, offset)
            }
            OpCode::SetLocal | OpCode::GetLocal => instruction.byte_instruction(self, offset),
            OpCode::Jump | OpCode::JumpIfFalse => instruction.jump_instruction(1, self, offset),
            OpCode::Loop => instruction.jump_instruction(-1, self, offset),
            _ => instruction.simple_instruction(offset),
        }
    }
}

struct Vm<'a> {
    pub chunk: Option<&'a Chunk>,
    pub ip: Option<std::iter::Peekable<std::iter::Enumerate<std::slice::Iter<'a, u8>>>>,
    pub stack: Vec<Value>,
    pub trace_execution: bool,
    pub globals: BTreeMap<String, Value>,
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
        if self.trace_execution {
            self.chunk.unwrap().disassemble("program");
        }
        loop {
            if self.trace_execution {
                print!("          ");
                for val in self.stack.iter().rev() {
                    print!("[ {:?} ]", val);
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
                OpCode::Pop => {
                    self.stack.pop().unwrap();
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    self.stack.push(self.stack[slot as usize].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = self.peek(0);
                }
                OpCode::GetGlobal => {
                    if let Value::String(s) = self.read_constant() {
                        match self.globals.get(&s) {
                            None => {
                                self.runtime_error(format!("Undefined variable '{}'.", s).as_str());
                                return InterpretResult::RuntimeError;
                            }
                            Some(v) => {
                                self.stack.push(v.clone());
                            }
                        }
                    }
                }
                OpCode::DefineGlobal => {
                    if let Value::String(s) = self.read_constant() {
                        self.globals.insert(s, self.stack.pop().unwrap());
                    }
                }
                OpCode::SetGlobal => {
                    if let Value::String(s) = self.read_constant() {
                        if let None = self.globals.insert(s.clone(), self.peek(0)) {
                            self.globals.remove(&s).unwrap();
                            self.runtime_error(format!("Undefined variable '{}'.", s).as_str());
                            return InterpretResult::RuntimeError;
                        }
                    }
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
                            }
                            _ => {
                                self.runtime_error("Operands must be two numbers or two strings.");
                                return InterpretResult::RuntimeError;
                            }
                        },
                        Value::Number(a) => match bv {
                            Value::Number(b) => {
                                self.stack.pop();
                                self.stack.pop();
                                self.stack.push(Value::Number(a + b));
                            }
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
                OpCode::Print => {
                    println!("{}", self.stack.pop().unwrap());
                }
                OpCode::Jump => {
                    let offset = self.read_short() as usize;
                    self.ip.as_mut().map(|x| x.nth(offset.sub(1)));
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short() as usize;
                    if self.peek(0).is_falsey() {
                        self.ip.as_mut().map(|x| x.nth(offset.sub(1)));
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_short() as usize;
                    let ip = self.ip.as_mut().unwrap().peek().unwrap().0;
                    self.ip = Some(self.chunk.unwrap().code.iter().enumerate().peekable());
                    self.ip.as_mut().map(|x| x.nth(ip.sub(offset).sub(1)));
                }
                OpCode::Return => {
                    return InterpretResult::Ok;
                }
            }
        }
    }
    fn read_byte(&mut self) -> u8 {
        *self.ip.as_mut().map(|x| x.next()).unwrap().unwrap().1
    }
    fn read_short(&mut self) -> u16 {
        let hi = self.read_byte();
        let lo = self.read_byte();
        (hi as u16) << 8 | (lo as u16)
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
        self.chunk.as_ref().unwrap().line
            [self.ip.as_mut().map_or(0, |x| x.peek().map_or(0, |x| x.0))]
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
        InterpretResult::CompileError => std::process::exit(65),
        InterpretResult::RuntimeError => std::process::exit(70),
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

fn interpret(source: &str, trace: bool) -> InterpretResult {
    match Parser::compile(source) {
        Ok(chunk) => {
            let mut vm = Vm {
                chunk: None,
                ip: None,
                stack: Vec::new(),
                trace_execution: trace,
                globals: BTreeMap::new(),
            };
            return vm.interpret(&chunk);
        }
        Err(e) => {
            return e;
        }
    }
}

struct Parser<'a> {
    compiler: Compiler<'a>,
    scanner: scanner::Scanner<'a>,
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    error: bool,
    panic_mode: bool,
    disassemble_on_error: bool,
}

enum FunctionType {
    Function,
    Script,
}

struct Compiler<'a> {
    function: Function,
    function_type: FunctionType,
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

impl<'a> Compiler<'a> {
    fn new() -> Compiler<'a> {
        Compiler {
            function: Function {
                arity: 0,
                chunk: Chunk::new(),
                name: None,
            },
            function_type: FunctionType::Script,
            locals: Vec::new(),
            scope_depth: 0,
        }
    }
}

struct Local<'a> {
    name: Token<'a>,
    depth: Option<usize>,
}

macro_rules! consume {
    ($self:expr, $token_type:ident, $error:expr) => {
        match $self.current.as_ref().unwrap().tokentype {
            TokenType::$token_type => $self.advance(),
            _ => $self.error_at_current($error),
        }
    };
}

macro_rules! advance_if {
    ($self:expr, $tt:tt) => {
        if let TokenType::$tt = $self.current.as_ref().unwrap().tokentype {
            $self.advance();
            true
        } else {
            false
        }
    };
}

macro_rules! check {
    ($self:expr, $tt:tt) => {
        if let TokenType::$tt = $self.current.as_ref().unwrap().tokentype {
            true
        } else {
            false
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
    prefix: Option<fn(&mut Parser, bool) -> ()>,
    infix: Option<fn(&mut Parser, bool) -> ()>,
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
        Some(|p: &mut Parser, ca: bool| p.$method(ca))
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
    variable, None,    None;       // Identifier
    string,   None,    None;       // String
    number,   None,    None;       // Number
    None,     and_,    And;        // And
    None,     None,    None;       // Class
    None,     None,    None;       // Else
    literal,  None,    None;       // False
    None,     None,    None;       // For
    None,     None,    None;       // Fun
    None,     None,    None;       // If
    literal,  None,    None;       // Nil
    None,     or_,     Or;         // Or
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
                    if self.current.is_some() {
                        self.error_at_current(format!("{}", e).as_str());
                    } else if !self.panic_mode {
                        self.panic_mode = true;
                        self.error = true;
                        eprintln!("{}", e);
                    }
                }
            }
        }
    }
    fn current_chunk(&self) -> &Chunk {
        &self.compiler.function.chunk
    }
    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
    }
    fn compile(source: &'a str) -> Result<Chunk, InterpretResult> {
        let mut parser: Parser = Parser {
            compiler: Compiler::new(),
            scanner: scanner::Scanner::new(source),
            current: None,
            previous: None,
            error: false,
            panic_mode: false,
            disassemble_on_error: false,
        };
        parser.advance();
        while !advance_if!(parser, EOF) {
            parser.declaration();
        }
        parser.emit_instr(OpCode::Return);
        if parser.error {
            if parser.disassemble_on_error {
                parser.current_chunk().disassemble("code");
            }
            Err(InterpretResult::CompileError)
        } else {
            Ok(parser.compiler.function.chunk)
        }
    }
    fn emit_instr(&mut self, instr: OpCode) {
        self.emit_byte(instr as u8)
    }
    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.as_ref().unwrap().line as usize;
        self.current_chunk_mut().write(byte, line);
    }
    fn emit_bytes(&mut self, byte: u8, byte2: u8) {
        self.emit_byte(byte);
        self.emit_byte(byte2);
    }
    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_instr(OpCode::Loop);

        let offset = self.current_chunk().code.len().sub(loop_start) + 2;
        if offset > std::u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }
    fn expression(&mut self) {
        self.parse_precedence(ParsePrecedence::Assignment)
    }
    fn declaration(&mut self) {
        if advance_if!(self, Var) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }
    fn statement(&mut self) {
        if advance_if!(self, Print) {
            self.print_statement();
        } else if advance_if!(self, LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if advance_if!(self, For) {
            self.for_statement();
        } else if advance_if!(self, If) {
            self.if_statement();
        } else if advance_if!(self, While) {
            self.while_statement();
        } else {
            self.expression_statement();
        }
    }
    fn block(&mut self) {
        while !check!(self, RightBrace) && !check!(self, EOF) {
            self.declaration();
        }
        consume!(self, RightBrace, "Expect '}' after block.");
    }
    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }
    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;
        while !self.compiler.locals.is_empty()
            && self.compiler.locals.last().unwrap().depth.is_some()
            && self.compiler.locals.last().unwrap().depth.unwrap() > self.compiler.scope_depth
        {
            self.emit_instr(OpCode::Pop);
            self.compiler.locals.pop();
        }
    }
    fn print_statement(&mut self) {
        self.expression();
        consume!(self, Semicolon, "Expect ';' after value.");
        self.emit_instr(OpCode::Print)
    }
    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();

        consume!(self, LeftParen, "Expect '(' after 'while'.");
        self.expression();
        consume!(self, RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        self.emit_instr(OpCode::Pop);
        self.statement();

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_instr(OpCode::Pop);
    }
    fn expression_statement(&mut self) {
        self.expression();
        consume!(self, Semicolon, "Expect ';' after expression.");
        self.emit_instr(OpCode::Pop);
    }
    fn for_statement(&mut self) {
        self.begin_scope();

        consume!(self, LeftParen, "Expect '(' after 'for'.");
        if advance_if!(self, Semicolon) {
        } else if advance_if!(self, Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();

        let mut exit_jump = None;
        if !advance_if!(self, Semicolon) {
            self.expression();
            consume!(self, Semicolon, "Expect ';' after loop condition.");
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse as u8));
            self.emit_instr(OpCode::Pop);
        }

        if !advance_if!(self, RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump as u8);

            let increment_start = self.current_chunk().code.len();
            self.expression();
            self.emit_instr(OpCode::Pop);
            consume!(self, RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        exit_jump.map(|x| {
            self.patch_jump(x);
            self.emit_instr(OpCode::Pop);
        });

        self.end_scope();
    }
    fn if_statement(&mut self) {
        consume!(self, LeftParen, "Expect '(' after 'if'.");
        self.expression();
        consume!(self, RightParen, "Expect ')' after condition.");
        let then_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit_instr(OpCode::Pop);
        self.statement();
        let else_jump = self.emit_jump(OpCode::Jump as u8);
        self.patch_jump(then_jump);
        self.emit_instr(OpCode::Pop);
        if advance_if!(self, Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }
    fn emit_jump(&mut self, instruction: u8) -> u16 {
        self.emit_byte(instruction);
        let dest = self.current_chunk().code.len() as u16;
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        dest
    }
    fn patch_jump(&mut self, offset: u16) {
        let jump: u16 = self.current_chunk().code.len() as u16 - offset - 2;
        self.current_chunk_mut().code[offset as usize] = (jump >> 8 & 0xff) as u8;
        self.current_chunk_mut().code[(offset as usize) + 1] = (jump & 0xff) as u8;
    }
    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");
        if advance_if!(self, Equal) {
            self.expression();
        } else {
            self.emit_instr(OpCode::Nil);
        }
        consume!(self, Semicolon, "Expect ';' after variable declaration.");
        self.define_variable(global);
    }
    fn parse_variable(&mut self, msg: &str) -> u8 {
        consume!(self, Identifier, msg);
        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }
        self.identifier_constant(self.previous.as_ref().unwrap().lexeme)
    }
    fn identifier_constant(&mut self, name: &str) -> u8 {
        self.make_constant(Value::String(name.to_string()))
    }
    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
    }
    fn and_(&mut self, _: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit_instr(OpCode::Pop);
        self.parse_precedence(ParsePrecedence::And);
        self.patch_jump(end_jump);
    }
    fn or_(&mut self, _: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        let end_jump = self.emit_jump(OpCode::Jump as u8);
        self.patch_jump(else_jump);
        self.emit_instr(OpCode::Pop);
        self.parse_precedence(ParsePrecedence::Or);
        self.patch_jump(end_jump);
    }
    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        let name = self.previous.as_ref().unwrap().lexeme;
        let depth = self.compiler.scope_depth;
        let mut error = false;
        for local in self.compiler.locals.iter().rev() {
            if let Some(d) = local.depth {
                if d < depth {
                    break;
                }
            }
            if name == local.name.lexeme {
                error = true;
            }
        }
        if error {
            self.error("Variable with this name already declared in this scope.");
        }
        let token = self.previous.take().unwrap(); // Danger!
        self.add_local(&token);
    }
    fn add_local(&mut self, name: &Token<'a>) {
        if self.compiler.locals.len() > std::u8::MAX as usize {
            self.error("Too many local variables in function.");
            return;
        }
        self.compiler.locals.push(Local {
            name: Token {
                tokentype: name.tokentype,
                lexeme: name.lexeme,
                line: name.line,
            },
            depth: None,
        })
    }
    fn mark_initialized(&mut self) {
        let depth = self.compiler.scope_depth;
        self.compiler.locals.last_mut().unwrap().depth = Some(depth);
    }
    fn number(&mut self, _: bool) {
        let value: f64 = self.previous.as_ref().unwrap().lexeme.parse().unwrap();
        self.emit_constant(Value::Number(value));
    }
    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, constant);
    }
    fn make_constant(&mut self, value: Value) -> u8 {
        match self.current_chunk_mut().add_constant(value) {
            Ok(c) => c,
            Err(_) => {
                self.error("Too many constants in one chunk");
                0
            }
        }
    }
    fn grouping(&mut self, _: bool) {
        self.expression();
        consume!(self, RightParen, "Expect ')' after expression.");
    }
    fn unary(&mut self, _: bool) {
        let prev = self.previous.take().unwrap(); // Danger!
        self.parse_precedence(ParsePrecedence::Unary);
        match prev.tokentype {
            TokenType::Bang => self.emit_instr(OpCode::Not),
            TokenType::Minus => self.emit_instr(OpCode::Negate),
            _ => (),
        }
    }
    fn binary(&mut self, _: bool) {
        let prev = self.previous.take().unwrap(); // Danger!
        let rule = ParseRule::get_rule(prev.tokentype);
        self.parse_precedence(ParsePrecedence::try_from(rule.precedence as u8 + 1).unwrap());
        match prev.tokentype {
            TokenType::Plus => self.emit_instr(OpCode::Add),
            TokenType::Minus => self.emit_instr(OpCode::Subtract),
            TokenType::Star => self.emit_instr(OpCode::Multiply),
            TokenType::Slash => self.emit_instr(OpCode::Divide),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit_instr(OpCode::Equal),
            TokenType::Greater => self.emit_instr(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_instr(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => (),
        }
    }
    fn literal(&mut self, _: bool) {
        match self.previous.as_ref().unwrap().tokentype {
            TokenType::False => self.emit_instr(OpCode::False),
            TokenType::Nil => self.emit_instr(OpCode::Nil),
            TokenType::True => self.emit_instr(OpCode::True),
            _ => (),
        }
    }
    fn string(&mut self, _: bool) {
        let lexeme = self.previous.as_ref().unwrap().lexeme;
        self.emit_constant(Value::String(lexeme[1..lexeme.len() - 1].to_string()));
    }
    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous.as_ref().unwrap().lexeme, can_assign)
    }
    fn named_variable(&mut self, name: &str, can_assign: bool) {
        let (arg, get_op, set_op) = if let Some(a) = self.resolve_local(name) {
            (a, OpCode::GetLocal as u8, OpCode::SetLocal as u8)
        } else {
            (
                self.identifier_constant(name),
                OpCode::GetGlobal as u8,
                OpCode::SetGlobal as u8,
            )
        };
        if can_assign && advance_if!(self, Equal) {
            self.expression();
            self.emit_bytes(set_op, arg);
        } else {
            self.emit_bytes(get_op, arg);
        }
    }
    fn resolve_local(&mut self, name: &str) -> Option<u8> {
        let mut error = false;
        let ret = self
            .compiler
            .locals
            .iter()
            .enumerate()
            .rev()
            .find_map(|(idx, local)| {
                if local.name.lexeme == name {
                    if local.depth.is_none() {
                        error = true;
                    }
                    Some(idx as u8)
                } else {
                    None
                }
            });
        if error {
            self.error("Cannot read local variable in its own initializer.");
        }
        ret
    }
    fn parse_precedence(&mut self, prec: ParsePrecedence) {
        self.advance();
        let rule = ParseRule::get_rule(self.previous.as_ref().unwrap().tokentype);
        match rule.prefix {
            None => self.error("Expect expression."),
            Some(f) => {
                let can_assign = prec <= ParsePrecedence::Assignment;
                f(self, can_assign);

                while prec
                    <= ParseRule::get_rule(self.current.as_ref().unwrap().tokentype).precedence
                {
                    self.advance();
                    let infix_rule =
                        ParseRule::get_rule(self.previous.as_ref().unwrap().tokentype).infix;
                    infix_rule.map(|f| f(self, can_assign));
                }

                if can_assign && advance_if!(self, Equal) {
                    self.error("Invalid assignment target.");
                }
            }
        }
    }
    fn synchronize(&mut self) {
        self.panic_mode = false;
        loop {
            if let TokenType::EOF = self.current.as_ref().unwrap().tokentype {
                return;
            }
            if let TokenType::Semicolon = self.previous.as_ref().unwrap().tokentype {
                return;
            }
            match self.current.as_ref().unwrap().tokentype {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
        }
    }
}
