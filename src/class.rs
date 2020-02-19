use crate::callable::LoxFunction;
use crate::environment::Environment;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Class<'a> {
    data: Rc<RefCell<ClassImpl<'a>>>,
}

#[derive(Debug)]
struct ClassImpl<'a> {
    name: &'a Token,
    methods: BTreeMap<String, LoxFunction<'a>>,
    environment: Environment<'a>,
}

impl<'a> Class<'a> {
    pub fn new(
        name: &'a Token,
        methods: BTreeMap<String, LoxFunction<'a>>,
        environment: Environment<'a>,
    ) -> Class<'a> {
        Class {
            data: Rc::new(RefCell::new(ClassImpl {
                name,
                methods,
                environment,
            })),
        }
    }
    pub fn find_method(&self, name: &str) -> Option<LoxFunction<'a>> {
        self.data.borrow().methods.get(name).map(|x| x.clone())
    }
    pub fn environment(&self) -> Environment<'a> {
        self.data.borrow().environment.clone()
    }
    pub fn equals(&self, other: &Class<'a>) -> bool {
        Rc::ptr_eq(&self.data, &other.data)
    }
}

impl<'a> fmt::Display for Class<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data.borrow().name.lexeme)
    }
}
