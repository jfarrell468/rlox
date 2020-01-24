use crate::ast::{Expression, Statement, Value, Visitor};
use crate::environment::Environment;
use crate::token::TokenType;
use std::rc::Rc;

pub struct Interpreter {
    pub environment: Environment,
}

impl Visitor<Expression, Value> for Interpreter {
    fn visit(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Literal(x) => match &x.tokentype {
                TokenType::String(y) => Value::String(y.clone()),
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
                    // Callable?
                    _ => Value::Nil,
                }
            }
            // TODO: Error handling.
            Expression::Variable(token) => self.environment.get(token.clone()).unwrap(),
            Expression::Assign { name, value } => {
                let value = self.evaluate(value);
                self.environment
                    .assign(name.clone(), value.clone())
                    .unwrap();
                value
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left);
                match operator.tokentype {
                    TokenType::Or => {
                        if is_truthy(&left) {
                            left
                        } else {
                            self.evaluate(right)
                        }
                    }
                    TokenType::And => {
                        if !is_truthy(&left) {
                            left
                        } else {
                            self.evaluate(right)
                        }
                    }
                    _ => Value::Nil,
                }
            }
            Expression::Call {
                callee,
                paren,
                arguments,
            } => {
                let evaluated_callee = self.evaluate(callee);
                let mut evaluated_arguments: Vec<Value> = Vec::new();
                for argument in arguments {
                    evaluated_arguments.push(self.evaluate(argument));
                }
                match evaluated_callee {
                    Value::Callable(function) => {
                        if function.arity() != evaluated_arguments.len() {
                            // TODO: Error handling.
                            Value::Nil
                        } else {
                            function.call(self, &evaluated_arguments);
                            Value::Nil
                        }
                    }
                    _ => Value::Nil,
                }
            }
        }
    }
}

impl Visitor<Statement, ()> for Interpreter {
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
                self.environment.define(name, val);
            }
            Statement::Block(stmts) => {
                self.environment.start_block();
                for stmt in stmts {
                    self.execute(stmt);
                }
                self.environment.end_block();
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&self.evaluate(condition)) {
                    self.execute(then_branch);
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch);
                }
            }
            Statement::While { condition, body } => {
                while is_truthy(&self.evaluate(condition)) {
                    self.execute(body);
                }
            }
            Statement::Function(callable) => self
                .environment
                .define(&callable.name, Value::Callable(callable.clone())),
            Statement::Return { keyword, value } => {
                let val = self.evaluate(value);
                unimplemented!("Return not implemented");
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
    pub fn execute(&mut self, stmt: &Statement) {
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
        Value::Callable(_) => false,
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
        _ => false,
    }
}
