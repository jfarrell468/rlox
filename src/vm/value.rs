use std::fmt;

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

impl<'a> fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Number(x) => {
                if x.is_sign_negative() {
                    write!(f, "-{}", -x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Value::String(x) => write!(f, "{}", x),
        }
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Boolean(x) => !*x,
            Value::Nil => true,
            Value::Number(_) => false,
            Value::String(_) => false,
        }
    }
    pub fn equals(&self, other: &Value) -> bool {
        match self {
            Value::Boolean(a) => match other {
                Value::Boolean(b) => a == b,
                _ => false,
            },
            Value::Nil => match other {
                Value::Nil => true,
                _ => false,
            },
            Value::Number(a) => match other {
                Value::Number(b) => a == b,
                _ => false,
            },
            Value::String(a) => match other {
                Value::String(b) => a == b,
                _ => false,
            },
        }
    }
}
