use crate::ast::Value;
use crate::token::Token;
use std::error::Error;
use std::fmt;

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct VariableError {
    token: Token,
}

impl<'a> fmt::Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] Error: Undefined variable '{}'",
            self.token.line, self.token.lexeme,
        )
    }
}

impl<'a> Error for VariableError {
    fn description(&self) -> &str {
        "Undefined variable"
    }
}

pub struct Environment {
    values: Vec<BTreeMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: vec![BTreeMap::new()],
        }
    }
    pub fn start_block(&mut self) {
        self.values.push(BTreeMap::new());
    }
    pub fn end_block(&mut self) {
        self.values.pop().unwrap();
    }
    pub fn define(&mut self, name: &String, value: Value) {
        self.values.last_mut().unwrap().insert(name.clone(), value);
    }
    pub fn get(&self, token: Token) -> Result<Value, Box<dyn Error>> {
        for cur in self.values.iter().rev() {
            if let Some(x) = cur.get(&token.lexeme) {
                return Ok(x.clone());
            }
        }
        Err(Box::new(VariableError { token: token }))
    }
    pub fn assign(&mut self, token: Token, value: Value) -> Result<(), Box<dyn Error>> {
        for cur in self.values.iter_mut().rev() {
            if let Some(x) = cur.get_mut(&token.lexeme) {
                *x = value;
                return Ok(());
            }
        }
        Err(Box::new(VariableError { token: token }))
    }
}
