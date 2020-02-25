use crate::ast::Value;
use crate::interpreter::ErrorType;
use crate::shared_list::SharedList;
use crate::token::Token;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct EnvironmentError<'a> {
    message: String,
    token: Option<&'a Token<'a>>,
}

impl<'a> fmt::Display for EnvironmentError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            None => write!(f, "Runtime error: {}", self.message),
            Some(token) => write!(f, "{}\n[line {}] ", self.message, token.line),
        }
    }
}

impl<'a> Error for EnvironmentError<'a> {
    fn description(&self) -> &str {
        &self.message
    }
}

type EnvList<'a> = SharedList<BTreeMap<String, Value<'a>>>;

#[derive(Clone, Debug)]
pub struct Environment<'a> {
    values: EnvList<'a>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        let mut env_list: EnvList = SharedList::new();
        env_list.push(BTreeMap::new());
        Environment { values: env_list }
    }
    pub fn new_child(&self) -> Environment<'a> {
        let mut env_list = self.values.clone();
        env_list.push(BTreeMap::new());
        Environment { values: env_list }
    }
    pub fn define(&mut self, name: String, value: Value<'a>) -> Result<(), ErrorType<'a>> {
        self.values.peek_mut().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |mut map| {
                (*map).insert(name.clone(), value);
                Ok(())
            },
        )
    }
    pub fn get_at(&self, token: &'a Token<'a>, distance: usize) -> Result<Value<'a>, ErrorType<'a>> {
        self.ancestor(distance).get_direct(token)
    }
    pub fn get_direct(&self, token: &'a Token<'a>) -> Result<Value<'a>, ErrorType<'a>> {
        self.values.peek().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |map| match (*map).get(token.lexeme) {
                Some(val) => Ok(val.clone()),
                None => Err(Environment::error(
                    format!("Undefined variable '{}'.", token.lexeme),
                    Some(token),
                )),
            },
        )
    }
    pub fn get_this_at(&self, distance: usize) -> Result<Value<'a>, ErrorType<'a>> {
        self.ancestor(distance).get_this()
    }
    pub fn get_this(&self) -> Result<Value<'a>, ErrorType<'a>> {
        self.values.peek().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |map| match (*map).get("this") {
                Some(val) => Ok(val.clone()),
                None => Err(Environment::error(
                    format!("Undefined variable '{}'.", "this"),
                    None,
                )),
            },
        )
    }
    pub fn assign_at(
        &mut self,
        token: &'a Token<'a>,
        value: Value<'a>,
        distance: usize,
    ) -> Result<(), ErrorType<'a>> {
        self.ancestor(distance).assign_direct(token, value)
    }
    pub fn assign_direct(
        &mut self,
        token: &'a Token<'a>,
        value: Value<'a>,
    ) -> Result<(), ErrorType<'a>> {
        self.values.peek_mut().map_or(
            Err(Environment::error("Empty environment".to_string(), None)),
            |mut map| match (*map).insert(token.lexeme.to_string(), value) {
                Some(_) => Ok(()),
                None => Err(Environment::error(
                    format!("Undefined variable '{}'.", token.lexeme),
                    Some(token),
                )),
            },
        )
    }
    fn ancestor(&self, distance: usize) -> Environment<'a> {
        let mut ancestor = self.values.clone();
        for _ in 0..distance {
            ancestor = ancestor.tail();
        }
        Environment { values: ancestor }
    }
    fn error(msg: String, token: Option<&'a Token<'a>>) -> ErrorType<'a> {
        ErrorType::EnvironmentError(EnvironmentError {
            message: msg,
            token: token,
        })
    }
    pub fn equals(&self, other: &Environment<'a>) -> bool {
        self.values.equals(&other.values)
    }
}
