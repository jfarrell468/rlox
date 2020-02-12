use crate::ast::Value;
use crate::interpreter::ErrorType;
use crate::shared_list::SharedList;
use crate::token::Token;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct EnvironmentError {
    message: String,
    token: Option<Token>,
}

impl<'a> fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            None => write!(f, "Runtime error: {}", self.message),
            Some(token) => write!(f, "{}\n[line {}] ", self.message, token.line),
        }
    }
}

impl<'a> Error for EnvironmentError {
    fn description(&self) -> &str {
        &self.message
    }
}

type EnvList = SharedList<BTreeMap<String, Value>>;

#[derive(Clone, Debug)]
pub struct Environment {
    values: EnvList,
}

impl Environment {
    pub fn new() -> Environment {
        let mut env_list: EnvList = SharedList::new();
        env_list.push(BTreeMap::new());
        Environment { values: env_list }
    }
    pub fn new_child(&self) -> Environment {
        let mut env_list = self.values.clone();
        env_list.push(BTreeMap::new());
        Environment { values: env_list }
    }
    pub fn define(&mut self, name: String, value: Value) -> Result<(), ErrorType> {
        self.values.peek_mut().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |mut map| match (*map).insert(name.clone(), value) {
                None => Ok(()),
                Some(_) => Err(Environment::error(
                    format!("Multiple definitions for {}", name),
                    None,
                )),
            },
        )
    }
    pub fn get_at(&self, token: &Token, distance: usize) -> Result<Value, ErrorType> {
        self.ancestor(distance).get_direct(token)
    }
    pub fn get_direct(&self, token: &Token) -> Result<Value, ErrorType> {
        self.values.peek().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |map| match (*map).get(&token.lexeme) {
                Some(val) => Ok(val.clone()),
                None => Err(Environment::error(
                    format!("Undefined variable '{}'.", token.lexeme),
                    Some(token.clone()),
                )),
            },
        )
    }
    pub fn assign_at(
        &mut self,
        token: Token,
        value: Value,
        distance: usize,
    ) -> Result<(), ErrorType> {
        self.ancestor(distance).assign_direct(token, value)
    }
    pub fn assign_direct(&mut self, token: Token, value: Value) -> Result<(), ErrorType> {
        self.values.peek_mut().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |mut map| match (*map).insert(token.lexeme.clone(), value) {
                Some(_) => Ok(()),
                None => Err(Environment::error(
                    format!("Undefined variable '{}'.", token.lexeme),
                    Some(token),
                )),
            },
        )
    }
    fn ancestor(&self, distance: usize) -> Environment {
        let mut ancestor = self.values.clone();
        for _ in 0..distance {
            ancestor = ancestor.tail();
        }
        Environment { values: ancestor }
    }
    fn error(msg: String, token: Option<Token>) -> ErrorType {
        ErrorType::EnvironmentError(EnvironmentError {
            message: msg,
            token: token,
        })
    }
}
