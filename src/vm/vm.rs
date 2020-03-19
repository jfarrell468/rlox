use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::ops::Sub;

use super::chunk::Chunk;
use super::compiler::InterpretResult;
use super::opcode::OpCode;
use super::value::Value;

struct Vm<'a> {
    chunk: Option<&'a Chunk>,
    ip: Option<std::iter::Peekable<std::iter::Enumerate<std::slice::Iter<'a, u8>>>>,
    stack: Vec<Value>,
    trace_execution: bool,
    globals: BTreeMap<String, Value>,
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

pub fn interpret(chunk: &Chunk, trace: bool) -> InterpretResult {
    let mut vm = Vm {
        chunk: Some(chunk),
        ip: Some(chunk.code.iter().enumerate().peekable()),
        stack: Vec::new(),
        trace_execution: trace,
        globals: BTreeMap::new(),
    };
    vm.run()
}

impl<'a> Vm<'a> {
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
        self.chunk.unwrap().constant(self.read_byte() as usize)
    }
    fn peek(&self, distance: usize) -> Value {
        self.stack
            .get(self.stack.len() - 1 - distance)
            .unwrap()
            .clone()
    }
    fn current_line(&mut self) -> usize {
        self.chunk.map_or(0, |chunk| chunk.line(self.ip.as_mut().map_or(0, |x| x.peek().map_or(0, |x| x.0))))
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
