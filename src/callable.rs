use crate::ast::{Statement, Value};
use crate::interpreter::{ErrorType, Interpreter};
use crate::token::Token;
use std::fmt;

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
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &Vec<Value>,
    ) -> Result<Value, ErrorType> {
        interpreter.environment.start_block();

        for param_and_val in self.params.iter().zip(arguments.iter()) {
            interpreter
                .environment
                .define(&param_and_val.0.lexeme, param_and_val.1.clone());
        }
        let result = interpreter.execute(&self.body);
        interpreter.environment.end_block();
        match result {
            Ok(v) => Ok(v),
            Err(e) => match e {
                ErrorType::Return(v) => Ok(v.0),
                _ => Err(e),
            },
        }
    }
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}
