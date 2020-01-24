use crate::ast::{Statement, Value};
use crate::interpreter::Interpreter;
use crate::token::Token;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct Callable {
    pub name: String,
    pub params: Vec<Token>,
    pub body: Statement,
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Callable {
    pub fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Value>) {
        interpreter.environment.start_block();

        for param_and_val in self.params.iter().zip(arguments.iter()) {
            interpreter
                .environment
                .define(&param_and_val.0.lexeme, param_and_val.1.clone());
        }
        interpreter.execute(&self.body);
        interpreter.environment.end_block();
    }
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}
