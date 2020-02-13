use crate::ast::Value;
use crate::class::Class;
use crate::interpreter::{ErrorType, RuntimeError};
use crate::token::Token;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Instance {
    data: Rc<RefCell<InstanceImpl>>,
}

#[derive(Debug)]
struct InstanceImpl {
    class: Class,
    fields: BTreeMap<String, Value>,
}

impl Instance {
    pub fn new(class: Class) -> Instance {
        Instance {
            data: Rc::new(RefCell::new(InstanceImpl {
                class,
                fields: BTreeMap::new(),
            })),
        }
    }
    pub fn get(&self, name: &Token) -> Result<Value, ErrorType> {
        self.data
            .borrow()
            .fields
            .get(name.lexeme.as_str())
            .map_or_else(
                || {
                    self.data
                        .borrow()
                        .class
                        .find_method(name.lexeme.as_str())
                        .map(|x| {
                            let mut env = self.data.borrow().class.environment().new_child();
                            env.define("this".to_string(), Value::Instance(self.clone()))
                                .unwrap();
                            Value::Callable(x.clone(), env)
                        })
                        .ok_or(RuntimeError::new(
                            format!("Undefined property '{}'.", name.lexeme).as_str(),
                            Some(name.clone()),
                        ))
                },
                |x| Ok(x.clone()),
            )
    }
    pub fn set(&mut self, name: Token, value: Value) {
        self.data.borrow_mut().fields.insert(name.lexeme, value);
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.data.borrow().class)
    }
}
