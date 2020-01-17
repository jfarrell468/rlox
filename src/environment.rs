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
    values: BTreeMap<String, Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: BTreeMap::new(),
        }
    }
    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }
    pub fn get<'a>(&self, token: &'a Token) -> Result<Value, Box<dyn Error + 'a>> {
        match self.values.get(token.lexeme) {
            None => Err(Box::new(VariableError { token: token })),
            Some(x) => Ok(x.clone()),
        }
    }
    pub fn assign<'a>(
        &mut self,
        token: &'a Token,
        value: Value,
    ) -> Result<(), Box<dyn Error + 'a>> {
        match self.values.get_mut(token.lexeme) {
            None => Err(Box::new(VariableError { token: token })),
            Some(x) => {
                *x = value;
                Ok(())
            }
        }
    }
}
