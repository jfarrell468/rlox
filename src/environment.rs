use super::ast::Value;
use super::token::Token;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct VariableError<'a> {
    token: &'a Token<'a>,
}

impl<'a> fmt::Display for VariableError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] Error: Undefined variable '{}'",
            self.token.line, self.token.lexeme,
        )
    }
}

impl<'a> Error for VariableError<'a> {
    fn description(&self) -> &str {
        "Undefined variable"
    }
}

#[derive(Debug)]
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
    pub fn define(&mut self, name: &str, value: Value) {
        self.values
            .last_mut()
            .unwrap()
            .insert(name.to_string(), value);
    }
    pub fn get<'b>(&self, token: &'b Token) -> Result<Value, Box<dyn Error + 'b>> {
        for cur in self.values.iter().rev() {
            if let Some(x) = cur.get(token.lexeme) {
                return Ok(x.clone());
            }
        }
        Err(Box::new(VariableError { token: token }))
    }
    pub fn assign<'b>(
        &mut self,
        token: &'b Token,
        value: Value,
    ) -> Result<(), Box<dyn Error + 'b>> {
        for cur in self.values.iter_mut().rev() {
            if let Some(x) = cur.get_mut(token.lexeme) {
                *x = value;
                return Ok(());
            }
        }
        Err(Box::new(VariableError { token: token }))
    }
}
