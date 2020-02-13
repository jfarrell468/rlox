use crate::ast::{Expression, Statement, Value, Visitor};
use crate::callable::{Callable, NativeFunction};
use crate::class::Class;
use crate::environment::{Environment, EnvironmentError};
use crate::instance::Instance;
use crate::token::{Token, TokenType};
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct Interpreter {
    globals: Environment,
    environment: Environment,
}

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
    token: Option<Token>,
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            None => write!(f, "{}", self.message),
            Some(token) => write!(f, "{}\n[line {}]", self.message, token.line),
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
    EnvironmentError(EnvironmentError),
    RuntimeError(RuntimeError),
}
impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::Return(x) => x.fmt(f),
            ErrorType::EnvironmentError(x) => x.fmt(f),
            ErrorType::RuntimeError(x) => x.fmt(f),
        }
    }
}
impl Error for ErrorType {
    fn description(&self) -> &str {
        match self {
            ErrorType::Return(x) => x.description(),
            ErrorType::EnvironmentError(x) => x.description(),
            ErrorType::RuntimeError(x) => x.description(),
        }
    }
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
                        _ => Err(RuntimeError::new(
                            "Operand must be a number.",
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
                match operator.tokentype {
                    TokenType::EqualEqual => Ok(Value::Boolean(is_equal(&lv, &rv))),
                    TokenType::BangEqual => Ok(Value::Boolean(!is_equal(&lv, &rv))),
                    TokenType::Plus => match &lv {
                        Value::Number(l) => match &rv {
                            Value::Number(r) => Ok(Value::Number(l + r)),
                            _ => Err(RuntimeError::new(
                                "Operands must be two numbers or two strings.",
                                Some(operator.clone()),
                            )),
                        },
                        Value::String(l) => match &rv {
                            Value::String(r) => {
                                let mut joined = l.clone();
                                joined.push_str(r.as_str());
                                Ok(Value::String(joined))
                            }
                            _ => Err(RuntimeError::new(
                                "Operands must be two numbers or two strings.",
                                Some(operator.clone()),
                            )),
                        },
                        _ => Err(RuntimeError::new(
                            "Operands must be two numbers or two strings.",
                            Some(operator.clone()),
                        )),
                    },
                    _ => match &lv {
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
                                _ => Err(RuntimeError::new(
                                    "Invalid numerical operand",
                                    Some(operator.clone()),
                                )),
                            },
                            _ => Err(RuntimeError::new(
                                "Operands must be numbers.",
                                Some(operator.clone()),
                            )),
                        },
                        _ => Err(RuntimeError::new(
                            "Operands must be numbers.",
                            Some(operator.clone()),
                        )),
                    },
                }
            }
            Expression::Variable { name, scope } => match scope {
                None => self.globals.get_direct(name),
                Some(distance) => self.environment.get_at(name, *distance),
            },
            Expression::Assign { name, value, scope } => {
                let value = self.evaluate(value)?;
                match scope {
                    None => self.globals.assign_direct(name.clone(), value.clone())?,
                    Some(distance) => {
                        self.environment
                            .assign_at(name.clone(), value.clone(), *distance)?
                    }
                }
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
                    Value::Callable(function, closure) => {
                        if function.arity() != evaluated_arguments.len() {
                            Err(RuntimeError::new(
                                format!(
                                    "Expected {} arguments but got {}.",
                                    function.arity(),
                                    evaluated_arguments.len()
                                )
                                .as_str(),
                                Some(paren.clone()),
                            ))
                        } else {
                            function.call(self, &evaluated_arguments, closure)
                        }
                    }
                    Value::NativeFunction(function) => {
                        if function.arity != evaluated_arguments.len() {
                            Err(RuntimeError::new(
                                format!(
                                    "Expected {} arguments but got {}.",
                                    function.arity,
                                    evaluated_arguments.len()
                                )
                                .as_str(),
                                Some(paren.clone()),
                            ))
                        } else {
                            Ok((function.call)(&evaluated_arguments))
                        }
                    }
                    Value::Class(class) => {
                        if evaluated_arguments.len() != 0 {
                            Err(RuntimeError::new(
                                format!(
                                    "Expected {} arguments but got {}.",
                                    0,
                                    evaluated_arguments.len()
                                )
                                .as_str(),
                                Some(paren.clone()),
                            ))
                        } else {
                            let instance = Instance::new(class.clone());
                            class.find_method("init").map(|x| {
                                let mut env = class.environment().new_child();
                                env.define("this".to_string(), Value::Instance(instance.clone()))
                                    .unwrap();
                                x.call(self, &vec![], env);
                            });
                            Ok(Value::Instance(instance))
                        }
                    }
                    _ => Err(RuntimeError::new(
                        "Can only call functions and classes.",
                        Some(paren.clone()),
                    )),
                }
            }
            Expression::Get { object, name } => {
                let object = self.evaluate(object)?;
                match object {
                    Value::Instance(instance) => instance.get(name),
                    _ => Err(RuntimeError::new(
                        "Only instances have properties.",
                        Some(name.clone()),
                    )),
                }
            }
            Expression::Set {
                object,
                name,
                value,
            } => {
                let mut obj = self.evaluate(object)?;
                if let Value::Instance(x) = &mut obj {
                    let value = self.evaluate(value)?;
                    x.set(name.clone(), value.clone());
                    Ok(value)
                } else {
                    Err(RuntimeError::new(
                        "Only instances have fields.",
                        Some(name.clone()),
                    ))
                }
            }
            Expression::This { token, scope } => self.environment.get_at(token, scope.unwrap()),
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
                self.environment.define(name.lexeme.clone(), val.clone())?;
                Ok(val)
            }
            Statement::Block(stmts) => self.execute_block(stmts, self.environment.new_child()),
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
                self.environment.define(
                    callable.name().lexeme.clone(),
                    Value::Callable(callable.clone(), self.environment.clone()),
                )?;
                Ok(Value::Nil)
            }
            Statement::Return { keyword: _, value } => {
                let val = self.evaluate(value)?;
                Err(ErrorType::Return(Return(val.clone())))
            }
            Statement::Class {
                name,
                methods: method_statements,
            } => {
                self.environment.define(name.lexeme.clone(), Value::Nil)?;
                let mut methods: BTreeMap<String, Callable> = BTreeMap::new();
                for method in method_statements {
                    if let Statement::Function(x) = method {
                        methods.insert(x.name().lexeme.clone(), x.clone());
                    }
                }
                self.environment.assign_direct(
                    name.clone(),
                    Value::Class(Class::new(name.clone(), methods, self.environment.clone())),
                )?;
                Ok(Value::Nil)
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env = Environment::new();
        env.define(
            "clock".to_string(),
            Value::NativeFunction(NativeFunction {
                call: |_: &Vec<Value>| -> Value {
                    Value::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_millis() as f64,
                    )
                },
                arity: 0,
            }),
        )
        .expect("Failed to define native functions");
        Interpreter {
            globals: env.clone(),
            environment: env,
        }
    }
    fn evaluate(&mut self, expr: &Expression) -> Result<Value, ErrorType> {
        expr.accept(self)
    }
    fn execute(&mut self, stmt: &Statement) -> Result<Value, ErrorType> {
        stmt.accept(self)
    }
    pub fn interpret(&mut self, statements: &Vec<Statement>) -> Result<Value, ErrorType> {
        let mut val = Value::Nil;
        for stmt in statements {
            val = self.execute(stmt)?;
        }
        Ok(val)
    }
    pub fn execute_block(
        &mut self,
        stmts: &Vec<Statement>,
        env: Environment,
    ) -> Result<Value, ErrorType> {
        let mut result = Ok(Value::Nil);
        let parent = self.environment.clone();
        self.environment = env;
        for stmt in stmts {
            result = self.execute(stmt);
            if let Err(_) = result {
                break;
            }
        }
        self.environment = parent;
        result
    }
}

fn is_truthy(x: &Value) -> bool {
    match x {
        Value::Nil => false,
        Value::Boolean(x) => *x,
        _ => true,
    }
}

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
        Value::Class(l) => match rv {
            Value::Class(r) => l.equals(r),
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod interpreter_tests {
    use crate::ast::Value;
    use crate::interpreter;
    use crate::parser;
    use crate::resolver;
    use crate::scanner;

    fn expect_number(source: &str, expected_value: f64) {
        let (tokens, success) = scanner::scan_tokens(source);
        assert!(success);
        let (mut statements, last_error) = parser::parse(&tokens);
        assert!(last_error.is_none());
        let mut resolver = resolver::Resolver::new();
        let result = resolver.resolve(&mut statements);
        assert!(result.is_ok(), "{}", result.err().unwrap());
        let mut interpreter = interpreter::Interpreter::new();
        let val = interpreter.interpret(&mut statements);
        assert!(val.is_ok(), "{}", val.err().unwrap());
        let val = val.unwrap();
        match val {
            Value::Number(x) => assert_eq!(x, expected_value),
            _ => panic!("Wrong type: {:?}", val),
        }
    }

    #[test]
    fn addition_statement() {
        expect_number("1+2;", 3.0);
    }

    #[test]
    fn variable_declaration() {
        expect_number("var x = 3;", 3.0);
        expect_number("var x = 3; x + 1;", 4.0);
    }

    #[test]
    fn variable_scope() {
        expect_number("var x = 1; { var x = 2; } x;", 1.0);
        expect_number("var x = 1; { var x = 2; x; }", 2.0);
    }

    #[test]
    fn while_loop() {
        expect_number(
            "var a = 0; var b = 1; while (a < 10000) { var temp = a; a = b; b = temp + b; } a;",
            10946.0,
        );
    }

    #[test]
    fn for_loop() {
        expect_number(
            r#"
var a = 1;
for(
  var b = 1;
  b <= 6;
  b = b + 1
) {
  a = a * b;
}
a;"#,
            720.0,
        )
    }

    #[test]
    fn function() {
        expect_number("fun square(x) { x * x; } square(2);", 4.0);
    }

    #[test]
    fn function_return() {
        expect_number(
            "fun sign(x) { if (x==0) { return 0; } if (x<0) { return -1; } return 1; } sign(-2);",
            -1.0,
        );
    }

    #[test]
    fn recursion() {
        expect_number(
            r#"fun factorial(x) { if(x<=1) { return 1; } return x * factorial(x-1); } factorial(5);"#,
            120.0,
        );
    }

    #[test]
    fn scope() {
        expect_number(
            "var a = 1; { fun get_a() { return a; } var a = 2; get_a(); }",
            1.0,
        )
    }
}
