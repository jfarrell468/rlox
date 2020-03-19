use num_enum::TryFromPrimitive;
use strum_macros::Display;

use super::chunk::Chunk;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Display)]
#[repr(u8)]
pub enum OpCode {
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
            chunk.constant(constant)
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
