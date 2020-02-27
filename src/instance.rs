use crate::ast::Value;
use crate::class::Class;
use crate::interpreter::{ErrorType, RuntimeError};
use crate::token::Token;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Instance<'a> {
    data: Rc<RefCell<InstanceImpl<'a>>>,
}

#[derive(Debug)]
struct InstanceImpl<'a> {
    class: Class<'a>,
    fields: BTreeMap<String, Value<'a>>,
}

impl<'a> Instance<'a> {
    pub fn new(class: Class<'a>) -> Instance<'a> {
        Instance {
            data: Rc::new(RefCell::new(InstanceImpl {
                class,
                fields: BTreeMap::new(),
            })),
        }
    }
    pub fn get(&self, name: &'a Token<'a>) -> Result<Value<'a>, ErrorType<'a>> {
        self.data.borrow().fields.get(name.lexeme).map_or_else(
            || {
                self.data
                    .borrow()
                    .class
                    .find_method(name.lexeme)
                    .map(|(f, e)| {
                        let mut env = e.new_child();
                        env.define("this".to_string(), Value::Instance(self.clone()))
                            .unwrap();
                        Value::Function(f.clone(), env, name.lexeme == "init")
                    })
                    .ok_or(RuntimeError::new(
                        format!("Undefined property '{}'.", name.lexeme).as_str(),
                        Some(name),
                    ))
            },
            |x| Ok(x.clone()),
        )
    }
    pub fn set(&mut self, name: &'a Token<'a>, value: Value<'a>) {
        self.data
            .borrow_mut()
            .fields
            .insert(name.lexeme.to_string(), value);
    }
    pub fn equals(&self, other: &Instance<'a>) -> bool {
        Rc::ptr_eq(&self.data, &other.data)
    }
}

impl<'a> fmt::Display for Instance<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.data.borrow().class)
    }
}
