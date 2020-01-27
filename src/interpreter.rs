use crate::ast::{Expression, Statement, Value, Visitor};
use crate::environment::{Environment, VariableError};
use crate::token::{Token, TokenType};
use std::error::Error;
use std::fmt;

pub struct Interpreter {
    pub environment: Environment,
}

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
    token: Option<Token>,
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            None => write!(f, "Runtime error: {}", self.message.as_str()),
            Some(token) => write!(
                f,
                "[line {}] Runtime error: {}\n  Context: {}",
                token.line, self.message, token.lexeme
            ),
        }
    }
}
impl Error for RuntimeError {
    fn description(&self) -> &str {
        &self.message
    }
}
impl RuntimeError {
    pub fn new(msg: &str, token: Option<Token>) -> ErrorType {
        ErrorType::RuntimeError(RuntimeError {
            message: msg.to_string(),
            token,
        })
    }
    pub fn type_error(expected: &Value, actual: &Value, token: Option<Token>) -> ErrorType {
        ErrorType::RuntimeError(RuntimeError {
            message: format!(
                "Type error: Expected {}, got {}",
                expected.type_str(),
                actual.type_str()
            ),
            token,
        })
    }
}

#[derive(Debug)]
pub struct Return(pub Value);
impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Error for Return {
    fn description(&self) -> &str {
        ""
    }
}

#[derive(Debug)]
pub enum ErrorType {
    Return(Return),
    VariableError(VariableError),
    RuntimeError(RuntimeError),
}

impl Visitor<Expression, Result<Value, ErrorType>> for Interpreter {
    fn visit(&mut self, expr: &Expression) -> Result<Value, ErrorType> {
        match expr {
            Expression::Literal(x) => Ok(match &x.tokentype {
                TokenType::String(y) => Value::String(y.clone()),
                TokenType::Number(y) => Value::Number(*y),
                TokenType::False => Value::Boolean(false),
                TokenType::True => Value::Boolean(true),
                _ => Value::Nil,
            }),
            Expression::Grouping(x) => self.evaluate(x),
            Expression::Unary { operator, right } => {
                let rv = self.evaluate(right)?;
                match operator.tokentype {
                    TokenType::Minus => match rv {
                        Value::Number(r) => Ok(Value::Number(-r)),
                        _ => Err(RuntimeError::type_error(
                            &Value::Number(0.0),
                            &rv,
                            Some(operator.clone()),
                        )),
                    },
                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(&rv))),
                    _ => Err(RuntimeError::new(
                        "Invalid unary operand",
                        Some(operator.clone()),
                    )),
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let lv = self.evaluate(left)?;
                let rv = self.evaluate(right)?;
                match &lv {
                    Value::Number(l) => match &rv {
                        Value::Number(r) => match operator.tokentype {
                            TokenType::Minus => Ok(Value::Number(l - r)),
                            TokenType::Slash => Ok(Value::Number(l / r)),
                            TokenType::Star => Ok(Value::Number(l * r)),
                            TokenType::Plus => Ok(Value::Number(l + r)),
                            TokenType::Greater => Ok(Value::Boolean(l > r)),
                            TokenType::GreaterEqual => Ok(Value::Boolean(l >= r)),
                            TokenType::Less => Ok(Value::Boolean(l < r)),
                            TokenType::LessEqual => Ok(Value::Boolean(l <= r)),
                            TokenType::EqualEqual => Ok(Value::Boolean(l == r)),
                            TokenType::BangEqual => Ok(Value::Boolean(l != r)),
                            _ => Err(RuntimeError::new(
                                "Invalid numerical operand",
                                Some(operator.clone()),
                            )),
                        },
                        _ => Err(RuntimeError::type_error(&lv, &rv, Some(operator.clone()))),
                    },
                    Value::String(l) => match &rv {
                        Value::String(r) => match operator.tokentype {
                            TokenType::Plus => {
                                let mut joined = l.clone();
                                joined.push_str(r.as_str());
                                Ok(Value::String(joined))
                            }
                            TokenType::EqualEqual => Ok(Value::Boolean(l == r)),
                            TokenType::BangEqual => Ok(Value::Boolean(l != r)),
                            _ => Err(RuntimeError::new(
                                "Invalid string operand",
                                Some(operator.clone()),
                            )),
                        },
                        _ => Err(RuntimeError::type_error(&lv, &rv, Some(operator.clone()))),
                    },
                    Value::Boolean(l) => match &rv {
                        Value::Boolean(r) => match operator.tokentype {
                            TokenType::EqualEqual => Ok(Value::Boolean(l == r)),
                            TokenType::BangEqual => Ok(Value::Boolean(l != r)),
                            _ => Err(RuntimeError::new(
                                "Invalid boolean operand",
                                Some(operator.clone()),
                            )),
                        },
                        _ => Err(RuntimeError::type_error(&lv, &rv, Some(operator.clone()))),
                    },
                    Value::Nil => match &rv {
                        Value::Nil => match operator.tokentype {
                            // nil == nil
                            TokenType::EqualEqual => Ok(Value::Boolean(true)),
                            TokenType::BangEqual => Ok(Value::Boolean(false)),
                            _ => Ok(Value::Nil),
                        },
                        _ => Ok(Value::Nil),
                    },
                    // Callable?
                    _ => Err(RuntimeError::new(
                        "Invalid binary expression",
                        Some(operator.clone()),
                    )),
                }
            }
            Expression::Variable(token) => self.environment.get(token.clone()),
            Expression::Assign { name, value } => {
                let value = self.evaluate(value)?;
                self.environment.assign(name.clone(), value.clone())?;
                Ok(value)
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left)?;
                match operator.tokentype {
                    TokenType::Or => {
                        if is_truthy(&left) {
                            Ok(left)
                        } else {
                            self.evaluate(right)
                        }
                    }
                    TokenType::And => {
                        if !is_truthy(&left) {
                            Ok(left)
                        } else {
                            self.evaluate(right)
                        }
                    }
                    _ => Err(RuntimeError::new(
                        "Invalid logical operand",
                        Some(operator.clone()),
                    )),
                }
            }
            Expression::Call {
                callee,
                paren,
                arguments,
            } => {
                let evaluated_callee = self.evaluate(callee)?;
                let mut evaluated_arguments: Vec<Value> = Vec::new();
                for argument in arguments {
                    evaluated_arguments.push(self.evaluate(argument)?);
                }
                match evaluated_callee {
                    Value::Callable(function) => {
                        if function.arity() != evaluated_arguments.len() {
                            Err(RuntimeError::new(
                                format!(
                                    "Wrong number of arguments to {}. Expected {}, got {}",
                                    function.name,
                                    function.arity(),
                                    evaluated_arguments.len()
                                )
                                .as_str(),
                                Some(paren.clone()),
                            ))
                        } else {
                            function.call(self, &evaluated_arguments)
                        }
                    }
                    _ => Err(RuntimeError::new(
                        format!("{} not callable", evaluated_callee.type_str()).as_str(),
                        Some(paren.clone()),
                    )),
                }
            }
        }
    }
}

impl Visitor<Statement, Result<Value, ErrorType>> for Interpreter {
    fn visit(&mut self, stmt: &Statement) -> Result<Value, ErrorType> {
        match stmt {
            Statement::Print(e) => {
                let val = self.evaluate(e)?;
                println!("{}", val);
                Ok(val)
            }
            Statement::Expression(e) => self.evaluate(e),
            Statement::Var { name, initializer } => {
                let val = self.evaluate(initializer)?;
                self.environment.define(name, val.clone());
                Ok(val)
            }
            Statement::Block(stmts) => {
                let mut result = Ok(Value::Nil);
                self.environment.start_block();
                for stmt in stmts {
                    result = self.execute(stmt);
                    if let Err(_) = result {
                        break;
                    }
                }
                self.environment.end_block();
                result
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.execute(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)
                } else {
                    Ok(Value::Nil)
                }
            }
            Statement::While { condition, body } => {
                let mut val = Value::Nil;
                while is_truthy(&self.evaluate(condition)?) {
                    val = self.execute(body)?.clone();
                }
                Ok(val)
            }
            Statement::Function(callable) => {
                self.environment
                    .define(&callable.name, Value::Callable(callable.clone()));
                Ok(Value::Nil)
            }
            Statement::Return { keyword: _, value } => {
                let val = self.evaluate(value)?;
                Err(ErrorType::Return(Return(val.clone())))
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
    fn evaluate(&mut self, expr: &Expression) -> Result<Value, ErrorType> {
        expr.accept(self)
    }
    pub fn execute(&mut self, stmt: &Statement) -> Result<Value, ErrorType> {
        stmt.accept(self)
    }
    pub fn interpret(&mut self, statements: &Vec<Statement>) -> Result<Value, ErrorType> {
        let mut val = Value::Nil;
        for stmt in statements {
            val = self.execute(stmt)?;
        }
        Ok(val)
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

#[cfg(test)]
mod interpreter_tests {
    use crate::scanner;
    use crate::parser;
    use crate::interpreter;
    use crate::ast::Value;

    #[test]
    fn basic_test() {
        let (tokens, success) = scanner::scan_tokens("1+2;");
        assert!(success);
        let result = parser::parse(&tokens);
        assert!(result.is_ok());
        let mut interpreter = interpreter::Interpreter::new();
        let val = interpreter.interpret(&result.unwrap());
        assert!(val.is_ok());
        match val.unwrap() {
            Value::Number(x) => assert_eq!(x, 3.0),
            _ => panic!("Wrong type.")
        }
    }
}