use super::ast::{Expression, Statement, Value, Visitor};
use super::environment::Environment;
use super::token::TokenType;

pub struct Interpreter {
    environment: Environment,
}
impl<'a> Visitor<Expression<'a>, Value> for Interpreter {
    fn visit(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Literal(x) => match x {
                TokenType::String(y) => Value::String(y.to_string()),
                TokenType::Number(y) => Value::Number(*y),
                TokenType::False => Value::Boolean(false),
                TokenType::True => Value::Boolean(true),
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
            Expression::Variable(token) => self.environment.get(*token).unwrap(),
        }
    }
}
impl<'a> Visitor<Statement<'a>, ()> for Interpreter {
    fn visit(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Print(e) => {
                let val = self.evaluate(e);
                println!("{}", val);
            }
            Statement::Expression(e) => {
                self.evaluate(e);
            }
            Statement::Var { name, initializer } => {
                let val = self.evaluate(initializer);
                self.environment.define(*name, val);
            }
        }
    }
}
impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(),
        }
    }
    fn evaluate(&mut self, expr: &Expression) -> Value {
        expr.accept(self)
    }
    fn execute(&mut self, stmt: &Statement) {
        stmt.accept(self);
    }
    pub fn interpret(&mut self, statements: &Vec<Statement>) {
        for stmt in statements {
            self.execute(stmt);
        }
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
