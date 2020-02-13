use crate::callable::Callable;
use crate::environment::Environment;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Class {
    data: Rc<RefCell<ClassImpl>>,
}

#[derive(Debug)]
struct ClassImpl {
    name: Token,
    methods: BTreeMap<String, Callable>,
    environment: Environment,
}

impl Class {
    pub fn new(
        name: Token,
        methods: BTreeMap<String, Callable>,
        environment: Environment,
    ) -> Class {
        Class {
            data: Rc::new(RefCell::new(ClassImpl {
                name,
                methods,
                environment,
            })),
        }
    }
    pub fn find_method(&self, name: &str) -> Option<Callable> {
        self.data.borrow().methods.get(name).map(|x| x.clone())
    }
    pub fn environment(&self) -> Environment {
        self.data.borrow().environment.clone()
    }
    pub fn equals(&self, other: &Class) -> bool {
        Rc::ptr_eq(&self.data, &other.data)
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data.borrow().name.lexeme)
    }
}
