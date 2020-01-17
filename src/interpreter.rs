use super::ast::{ExprVisitor, Expression};
use super::token::TokenType;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
        }
    }
}

pub struct Interpreter {}
impl ExprVisitor<Value> for Interpreter {
    fn visit(&self, expr: &Expression) -> Value {
        match expr {
            Expression::Literal(x) => match x {
                TokenType::String(y) => Value::String(y.to_string()),
                TokenType::Number(y) => Value::Number(*y),
                _ => Value::Nil,
            },
            Expression::Grouping(x) => self.evaluate(x),
            Expression::Unary { operator, right } => {
                let rv = self.evaluate(right);
                match operator.tokentype {
                    TokenType::Minus => match rv {
                        Value::Number(r) => Value::Number(-r),
                        _ => Value::Nil,
                    },
                    TokenType::Bang => Value::Boolean(!is_truthy(&rv)),
                    _ => Value::Nil,
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let lv = self.evaluate(left);
                let rv = self.evaluate(right);
                match lv {
                    Value::Number(l) => match rv {
                        Value::Number(r) => match operator.tokentype {
                            TokenType::Minus => Value::Number(l - r),
                            TokenType::Slash => Value::Number(l / r),
                            TokenType::Star => Value::Number(l * r),
                            TokenType::Plus => Value::Number(l + r),
                            TokenType::Greater => Value::Boolean(l > r),
                            TokenType::GreaterEqual => Value::Boolean(l >= r),
                            TokenType::Less => Value::Boolean(l < r),
                            TokenType::LessEqual => Value::Boolean(l <= r),
                            TokenType::EqualEqual => Value::Boolean(l == r),
                            TokenType::BangEqual => Value::Boolean(l != r),
                            _ => Value::Nil,
                        },
                        _ => Value::Nil,
                    },
                    Value::String(l) => match rv {
                        Value::String(r) => match operator.tokentype {
                            TokenType::Plus => {
                                let mut joined = l;
                                joined.push_str(r.as_str());
                                Value::String(joined)
                            }
                            TokenType::EqualEqual => Value::Boolean(l == r),
                            TokenType::BangEqual => Value::Boolean(l != r),
                            _ => Value::Nil,
                        },
                        _ => Value::Nil,
                    },
                    Value::Boolean(l) => match rv {
                        Value::Boolean(r) => match operator.tokentype {
                            TokenType::EqualEqual => Value::Boolean(l == r),
                            TokenType::BangEqual => Value::Boolean(l != r),
                            _ => Value::Nil,
                        },
                        _ => Value::Nil,
                    },
                    Value::Nil => match rv {
                        Value::Nil => match operator.tokentype {
                            // nil == nil
                            TokenType::EqualEqual => Value::Boolean(true),
                            TokenType::BangEqual => Value::Boolean(false),
                            _ => Value::Nil,
                        },
                        _ => Value::Nil,
                    },
                }
            }
        }
    }
}
impl Interpreter {
    fn evaluate(&self, n: &Expression) -> Value {
        n.accept(self)
    }
}
fn is_truthy(x: &Value) -> bool {
    match x {
        Value::Nil => false,
        Value::Boolean(x) => *x,
        Value::Number(_) => true,
        Value::String(_) => true,
    }
}
// TODO: This isn't wired in correctly.
fn is_equal(lv: &Value, rv: &Value) -> bool {
    match lv {
        Value::Nil => match rv {
            Value::Nil => true,
            _ => false,
        },
        Value::Boolean(l) => match rv {
            Value::Boolean(r) => l == r,
            _ => false,
        },
        Value::Number(l) => match rv {
            Value::Number(r) => l == r,
            _ => false,
        },
        Value::String(l) => match rv {
            Value::String(r) => l == r,
            _ => false,
        },
    }
}
