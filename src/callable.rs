use crate::ast::{Statement, Value};
use crate::environment::Environment;
use crate::interpreter::{ErrorType, Interpreter, RuntimeError};
use crate::token::Token;
use std::cell::{Ref, RefCell, RefMut};
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Callable {
    data: Rc<RefCell<CallableImpl>>,
}

#[derive(Debug)]
struct CallableImpl {
    name: Token,
    params: Vec<Token>,
    body: Statement,
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name().lexeme)
    }
}

impl Callable {
    pub fn new(name: Token, params: Vec<Token>, body: Statement) -> Callable {
        Callable {
            data: Rc::new(RefCell::new(CallableImpl { name, params, body })),
        }
    }
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
        closure: Environment,
    ) -> Result<Value, ErrorType> {
        let mut environment = closure.new_child();

        for param_and_val in self.params().iter().zip(arguments.iter()) {
            environment.define(param_and_val.0.lexeme.clone(), param_and_val.1.clone())?;
        }
        match &self.data.borrow().body {
            Statement::Block(stmts) => {
                let result = interpreter.execute_block(stmts, environment.new_child());
                match result {
                    Ok(v) => Ok(v),
                    Err(e) => match e {
                        ErrorType::Return(v) => Ok(v.0),
                        _ => Err(e),
                    },
                }
            }
            _ => Err(RuntimeError::new("Function body has wrong type", None)),
        }
    }
    pub fn arity(&self) -> usize {
        self.data.borrow().params.len()
    }
    pub fn name(&self) -> Ref<Token> {
        Ref::map(self.data.borrow(), |data| &data.name)
    }
    pub fn params(&self) -> Ref<Vec<Token>> {
        Ref::map(self.data.borrow(), |data| &data.params)
    }
    pub fn body(&mut self) -> Ref<Statement> {
        Ref::map(self.data.borrow(), |data| &data.body)
    }
    pub fn body_mut(&mut self) -> RefMut<Statement> {
        RefMut::map(self.data.borrow_mut(), |data| &mut data.body)
    }
}
