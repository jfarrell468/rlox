use crate::ast::{Statement, Value};
use crate::environment::Environment;
use crate::interpreter::{ErrorType, Interpreter};
use crate::token::Token;
use std::cell::{Ref, RefCell, RefMut};
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct LoxFunction<'a> {
    data: Rc<RefCell<LoxFunctionImpl<'a>>>,
}

#[derive(Debug)]
struct LoxFunctionImpl<'a> {
    name: &'a Token,
    params: Vec<&'a Token>,
    body: Vec<Statement<'a>>,
}

impl<'a> fmt::Display for LoxFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name().lexeme)
    }
}

impl<'a> LoxFunction<'a> {
    pub fn new(name: &'a Token, params: Vec<&'a Token>, body: Vec<Statement<'a>>) -> LoxFunction<'a> {
        LoxFunction {
            data: Rc::new(RefCell::new(LoxFunctionImpl { name, params, body })),
        }
    }
    pub fn call(
        &self,
        interpreter: &mut Interpreter<'a>,
        arguments: &Vec<Value<'a>>,
        closure: Environment<'a>,
    ) -> Result<Value<'a>, ErrorType<'a>> {
        let mut environment = closure.new_child();

        for param_and_val in self.params().iter().zip(arguments.iter()) {
            environment.define(param_and_val.0.lexeme.clone(), param_and_val.1.clone())?;
        }
        let result = interpreter.execute_block(&*self.body(), environment);
        match result {
            Ok(v) => Ok(v),
            Err(e) => match e {
                ErrorType::Return(v) => Ok(v.0),
                _ => Err(e),
            },
        }
    }
    pub fn arity(&self) -> usize {
        self.data.borrow().params.len()
    }
    pub fn name(&self) -> Ref<&'a Token> {
        Ref::map(self.data.borrow(), |data| &data.name)
    }
    pub fn params(&self) -> Ref<Vec<&'a Token>> {
        Ref::map(self.data.borrow(), |data| &data.params)
    }
    pub fn body(&self) -> Ref<Vec<Statement<'a>>> {
        Ref::map(self.data.borrow(), |data| &data.body)
    }
    pub fn body_mut(&mut self) -> RefMut<Vec<Statement<'a>>> {
        RefMut::map(self.data.borrow_mut(), |data| &mut data.body)
    }
}

#[derive(Clone)]
pub struct NativeFunction<'a> {
    pub call: fn(&Vec<Value<'a>>) -> Value<'a>,
    pub arity: usize,
}

impl<'a> Debug for NativeFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl<'a> fmt::Display for NativeFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}
