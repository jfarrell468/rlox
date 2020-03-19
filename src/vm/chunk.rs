use std::convert::{TryFrom, TryInto};

use super::opcode::OpCode;
use super::value::Value;

pub struct Chunk {
    // TODO: make private
    pub code: Vec<u8>,
    constant: Vec<Value>,
    line: Vec<usize>,
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
    pub fn line(&self, ip: usize) -> usize {
        self.line[ip]
    }
    pub fn constant(&self, idx: usize) -> Value {
        self.constant.get(idx).unwrap().clone()
    }
    pub fn len(&self) -> usize {
        self.code.len()
    }
}
