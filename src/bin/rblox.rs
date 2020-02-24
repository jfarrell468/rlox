use num_enum::TryFromPrimitive;
use strum_macros::Display;
use std::convert::TryFrom;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Display)]
#[repr(u8)]
enum OpCode {
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

impl OpCode {
    pub fn simple_instruction(&self, offset: usize) -> usize {
        println!("{}", self);
        offset+1
    }
    pub fn constant_instruction(&self, chunk: &Chunk, offset: usize) -> usize {
        let constant = chunk.code[offset + 1] as usize;
        println!("{:16} {:4} '{}'", format!("{}", self), constant, chunk.constant[constant]);
        offset+2
    }
}

type Value = f64;

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
            line: Vec::new()
        }
    }
    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.line.push(line);
    }
    pub fn add_constant(&mut self, value: Value) -> u8 {
        let idx = self.constant.len() as u8;
        self.constant.push(value);
        idx
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
            OpCode::Return | OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => instruction.simple_instruction(offset),
            OpCode::Constant => instruction.constant_instruction(self, offset)
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
    ($stack:expr, $op:tt) => {
        let b = $stack.pop().unwrap();
        let a = $stack.pop().unwrap();
        $stack.push(a $op b);
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
                self.chunk.unwrap().disassemble_instruction(self.ip.as_mut().map(|x| x.peek()).unwrap().unwrap().0);
            }
            let instruction = OpCode::try_from(self.read_byte()).unwrap();
            match instruction {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                    println!("{}", constant);
                },
                OpCode::Add => {
                    binary_op!(self.stack, +);
                },
                OpCode::Subtract => {
                    binary_op!(self.stack, -);
                },
                OpCode::Multiply => {
                    binary_op!(self.stack, *);
                },
                OpCode::Divide => {
                    binary_op!(self.stack, /);
                },
                OpCode::Negate => {
                    let val = -self.stack.pop().unwrap();
                    self.stack.push(val);
                }
                OpCode::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    return InterpretResult::Ok;
                },
            }
        }
    }
    fn read_byte(&mut self) -> u8 {
        *self.ip.as_mut().map(|x| x.next()).unwrap().unwrap().1
    }
    fn read_constant(&mut self) -> Value {
        self.chunk.unwrap().constant[self.read_byte() as usize]
    }
}

fn main() {
    let mut c = Chunk::new();

    let constant = c.add_constant(1.2);
    c.write(OpCode::Constant as u8, 123);
    c.write(constant, 123);

    let constant = c.add_constant(3.4);
    c.write(OpCode::Constant as u8, 123);
    c.write(constant, 123);

    c.write(OpCode::Add as u8, 123);

    let constant = c.add_constant(5.6);
    c.write(OpCode::Constant as u8, 123);
    c.write(constant, 123);

    c.write(OpCode::Divide as u8, 123);
    c.write(OpCode::Negate as u8, 123);

    c.write(OpCode::Return as u8, 123);
    c.disassemble("test chunk");
    let mut vm = Vm{
        chunk:None,
        ip:None,
        stack: Vec::new(),
        trace_execution: true
    };
    vm.interpret(&c);
}