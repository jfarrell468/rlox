use crate::ast::{Expression, Statement, Value, Visitor};
use crate::callable::{LoxFunction, NativeFunction};
use crate::class::Class;
use crate::environment::{Environment, EnvironmentError};
use crate::instance::Instance;
use crate::token::{Token, TokenType};
use std::collections::BTreeMap;
use std::fmt;
use std::io;
use std::io::Read;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct Interpreter<'a> {
    globals: Environment<'a>,
    environment: Environment<'a>,
    out: &'a mut dyn io::Write,
}

#[derive(Debug)]
pub struct RuntimeError<'a> {
    message: String,
    token: Option<&'a Token<'a>>,
}
impl<'a> fmt::Display for RuntimeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            None => write!(f, "{}", self.message),
            Some(token) => write!(f, "{}\n[line {}]", self.message, token.line),
        }
    }
}
impl<'a> RuntimeError<'a> {
    pub fn new(msg: &str, token: Option<&'a Token<'a>>) -> ErrorType<'a> {
        ErrorType::RuntimeError(RuntimeError {
            message: msg.to_string(),
            token,
        })
    }
}

#[derive(Debug)]
pub struct Return<'a>(pub Value<'a>);
impl<'a> fmt::Display for Return<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {}", self.0)
    }
}

#[derive(Debug)]
pub enum ErrorType<'a> {
    Return(Return<'a>),
    EnvironmentError(EnvironmentError<'a>),
    RuntimeError(RuntimeError<'a>),
}
impl<'a> fmt::Display for ErrorType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::Return(x) => x.fmt(f),
            ErrorType::EnvironmentError(x) => x.fmt(f),
            ErrorType::RuntimeError(x) => x.fmt(f),
        }
    }
}

impl<'a> Visitor<Expression<'a>, Result<Value<'a>, ErrorType<'a>>> for Interpreter<'a> {
    fn visit(&mut self, expr: &Expression<'a>) -> Result<Value<'a>, ErrorType<'a>> {
        match expr {
            Expression::Literal(x) => Ok(match &x.tokentype {
                TokenType::String => Value::String(x.lexeme[1..x.lexeme.len() - 1].to_string()),
                TokenType::Number => Value::Number(x.lexeme.parse().unwrap()),
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
                            Some(operator),
                        )),
                    },
                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(&rv))),
                    _ => Err(RuntimeError::new("Invalid unary operand", Some(operator))),
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
                                Some(operator),
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
                                Some(operator),
                            )),
                        },
                        _ => Err(RuntimeError::new(
                            "Operands must be two numbers or two strings.",
                            Some(operator),
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
                                    Some(operator),
                                )),
                            },
                            _ => Err(RuntimeError::new(
                                "Operands must be numbers.",
                                Some(operator),
                            )),
                        },
                        _ => Err(RuntimeError::new(
                            "Operands must be numbers.",
                            Some(operator),
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
                    None => self.globals.assign_direct(name, value.clone())?,
                    Some(distance) => self.environment.assign_at(name, value.clone(), *distance)?,
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
                    _ => Err(RuntimeError::new("Invalid logical operand", Some(operator))),
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
                    Value::Function(function, closure, is_init) => {
                        if function.arity() != evaluated_arguments.len() {
                            Err(RuntimeError::new(
                                format!(
                                    "Expected {} arguments but got {}.",
                                    function.arity(),
                                    evaluated_arguments.len()
                                )
                                .as_str(),
                                Some(paren),
                            ))
                        } else {
                            function.call(self, &evaluated_arguments, closure, is_init)
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
                                Some(paren),
                            ))
                        } else {
                            Ok((function.call)(&evaluated_arguments))
                        }
                    }
                    Value::Class(class) => {
                        let arity = class.find_method("init").map_or(0, |(f, _)| f.arity());
                        if evaluated_arguments.len() != arity {
                            Err(RuntimeError::new(
                                format!(
                                    "Expected {} arguments but got {}.",
                                    arity,
                                    evaluated_arguments.len()
                                )
                                .as_str(),
                                Some(paren),
                            ))
                        } else {
                            let instance = Instance::new(class.clone());
                            class.find_method("init").map(|(f, e)| {
                                let mut env = e.new_child();
                                env.define("this".to_string(), Value::Instance(instance.clone()))
                                    .unwrap();
                                f.call(self, &evaluated_arguments, env, true).unwrap();
                            });
                            Ok(Value::Instance(instance))
                        }
                    }
                    _ => Err(RuntimeError::new(
                        "Can only call functions and classes.",
                        Some(paren),
                    )),
                }
            }
            Expression::Get { object, name } => {
                let object = self.evaluate(object)?;
                match object {
                    Value::Instance(instance) => instance.get(name),
                    _ => Err(RuntimeError::new(
                        "Only instances have properties.",
                        Some(name),
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
                    x.set(name, value.clone());
                    Ok(value)
                } else {
                    Err(RuntimeError::new("Only instances have fields.", Some(name)))
                }
            }
            Expression::This { token, scope } => self.environment.get_at(token, scope.unwrap()),
            Expression::Super {
                keyword,
                method,
                scope,
            } => {
                let superclass = self.environment.get_at(keyword, scope.unwrap())?;
                let object = self.environment.get_this_at(scope.unwrap() - 1)?;
                if let Value::Class(sc) = superclass {
                    sc.find_method(method.lexeme).map_or(
                        Err(RuntimeError::new(
                            format!("Undefined property '{}'.", method.lexeme).as_str(),
                            Some(keyword),
                        )),
                        |(f, e)| {
                            let mut env = e.new_child();
                            env.define("this".to_string(), object).unwrap();
                            Ok(Value::Function(f.clone(), env, method.lexeme == "init"))
                        },
                    )
                } else {
                    Err(RuntimeError::new(
                        "Wrong type for superclass value",
                        Some(keyword),
                    ))
                }
            }
        }
    }
}

impl<'a> Visitor<Statement<'a>, Result<Value<'a>, ErrorType<'a>>> for Interpreter<'a> {
    fn visit(&mut self, stmt: &Statement<'a>) -> Result<Value<'a>, ErrorType<'a>> {
        match stmt {
            Statement::Print(e) => {
                let val = self.evaluate(e)?;
                writeln!(self.out, "{}", val).unwrap();
            }
            Statement::Expression(e) => {
                self.evaluate(e)?;
            }
            Statement::Var { name, initializer } => {
                let val = match initializer {
                    None => Value::Nil,
                    Some(x) => self.evaluate(x)?,
                };
                self.environment
                    .define(name.lexeme.to_string(), val.clone())?;
            }
            Statement::Block(stmts) => {
                self.execute_block(stmts, self.environment.new_child())?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }
            Statement::While { condition, body } => loop {
                if let Some(x) = condition {
                    if !is_truthy(&self.evaluate(x)?) {
                        break;
                    }
                }
                self.execute(body)?;
            },
            Statement::Function(function) => {
                self.environment.define(
                    function.name().lexeme.to_string(),
                    Value::Function(function.clone(), self.environment.clone(), false),
                )?;
            }
            Statement::Return { keyword: _, value } => {
                let val = match value {
                    None => Value::Nil,
                    Some(x) => self.evaluate(x)?,
                };
                return Err(ErrorType::Return(Return(val.clone())));
            }
            Statement::Class {
                name,
                superclass,
                methods: method_statements,
            } => {
                let mut superclass_class: Option<Class> = None;
                if let Some(superclass) = superclass {
                    if let Expression::Variable {
                        name: superclass_name,
                        scope: _,
                    } = superclass
                    {
                        if let Value::Class(sc) = self.evaluate(superclass)? {
                            superclass_class = Some(sc);
                        } else {
                            return Err(RuntimeError::new(
                                "Superclass must be a class.",
                                Some(superclass_name),
                            ));
                        }
                    } else {
                        return Err(RuntimeError::new(
                            "Wrong type for superclass expression.",
                            None,
                        ));
                    }
                }
                self.environment
                    .define(name.lexeme.to_string(), Value::Nil)?;
                let mut environment = self.environment.clone();
                if let Some(superclass) = &superclass_class {
                    environment = environment.new_child();
                    environment.define("super".to_string(), Value::Class(superclass.clone()))?;
                }
                let mut methods: BTreeMap<String, LoxFunction> = BTreeMap::new();
                for method in method_statements {
                    if let Statement::Function(x) = method {
                        methods.insert(x.name().lexeme.to_string(), x.clone());
                    }
                }
                let class = Value::Class(Class::new(
                    name,
                    superclass_class,
                    methods,
                    environment.clone(),
                ));
                self.environment.assign_direct(name, class)?;
            }
        }
        Ok(Value::Nil)
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(out: &'a mut dyn io::Write) -> Interpreter<'a> {
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
        .expect("Failed to define native function 'clock'");
        env.define(
            "getc".to_string(),
            Value::NativeFunction(NativeFunction {
                call: |_: &Vec<Value>| -> Value {
                    let mut buffer = [0; 1];
                    Value::Number(match io::stdin().read(&mut buffer) {
                        Ok(1) => buffer[0] as f64,
                        _ => -1.0,
                    })
                },
                arity: 0,
            }),
        )
        .expect("Failed to define native function 'getc'");
        env.define(
            "chr".to_string(),
            Value::NativeFunction(NativeFunction {
                call: |args: &Vec<Value>| -> Value {
                    if let Value::Number(x) = args[0] {
                        if let Some(x) = std::char::from_u32(x as u32) {
                            let mut y = String::from("");
                            y.push(x);
                            return Value::String(y);
                        }
                    }
                    Value::Nil
                },
                arity: 1,
            }),
        )
        .expect("Failed to define native function 'chr'");
        env.define(
            "exit".to_string(),
            Value::NativeFunction(NativeFunction {
                call: |args: &Vec<Value>| -> Value {
                    std::process::exit(match args[0] {
                        Value::Number(x) => x as i32,
                        _ => 0,
                    });
                },
                arity: 1,
            }),
        )
        .expect("Failed to define native function 'exit'");
        env.define(
            "print_error".to_string(),
            Value::NativeFunction(NativeFunction {
                call: |args: &Vec<Value>| -> Value {
                    if let Value::String(x) = &args[0] {
                        eprintln!("{}", x);
                    }
                    Value::Nil
                },
                arity: 1,
            }),
        )
        .expect("Failed to define native function 'print_error'");
        Interpreter {
            globals: env.clone(),
            environment: env,
            out: out,
        }
    }
    fn evaluate(&mut self, expr: &Expression<'a>) -> Result<Value<'a>, ErrorType<'a>> {
        expr.accept(self)
    }
    fn execute(&mut self, stmt: &Statement<'a>) -> Result<Value<'a>, ErrorType<'a>> {
        stmt.accept(self)
    }
    pub fn interpret(
        &mut self,
        statements: &Vec<Statement<'a>>,
    ) -> Result<Value<'a>, ErrorType<'a>> {
        let mut val = Value::Nil;
        for stmt in statements {
            val = self.execute(stmt)?;
        }
        Ok(val)
    }
    pub fn execute_block(
        &mut self,
        stmts: &Vec<Statement<'a>>,
        env: Environment<'a>,
    ) -> Result<Value<'a>, ErrorType<'a>> {
        let mut result = Ok(Value::Nil);
        let parent = self.environment.clone();
        self.environment = env;
        for stmt in stmts {
            result = self.execute(stmt);
            if let Err(_) = &result {
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

fn is_equal<'a>(lv: &Value<'a>, rv: &Value<'a>) -> bool {
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
        Value::Instance(l) => match rv {
            Value::Instance(r) => l.equals(r),
            _ => false,
        },
        Value::Function(l, le, _) => match rv {
            Value::Function(r, re, _) => l.equals(r) && le.equals(re),
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod interpreter_tests {
    use crate::interpreter;
    use crate::parser;
    use crate::resolver;
    use crate::scanner;

    fn expect_output(source: &str, expected_output: &str) {
        let (tokens, success) = scanner::scan_tokens(source);
        assert!(success);
        let (mut statements, last_error) = parser::parse(&tokens);
        assert!(last_error.is_none());
        let mut resolver = resolver::Resolver::new();
        let result = resolver.resolve(&mut statements);
        assert!(result.is_ok(), "{}", result.err().unwrap());
        let mut out = Vec::new();
        {
            let mut interpreter = interpreter::Interpreter::new(&mut out);
            let val = interpreter.interpret(&mut statements);
            assert!(val.is_ok(), "{}", val.err().unwrap());
        }
        assert_eq!(
            out.as_slice(),
            expected_output.as_bytes(),
            "{} != {}",
            String::from_utf8(out.clone()).unwrap(),
            expected_output
        )
    }

    #[test]
    fn addition_statement() {
        expect_output("print 1+2;", "3\n");
    }

    #[test]
    fn variable_declaration() {
        expect_output("var x = 3; print x;", "3\n");
        expect_output("var x = 3; print x + 1;", "4\n");
    }

    #[test]
    fn variable_scope() {
        expect_output("var x = 1; { var x = 2; } print x;", "1\n");
        expect_output("var x = 1; { var x = 2; print x; }", "2\n");
    }

    #[test]
    fn while_loop() {
        expect_output(
            "var a = 0; var b = 1; while (a < 10000) { var temp = a; a = b; b = temp + b; } print a;",
            "10946\n",
        );
    }

    #[test]
    fn for_loop() {
        expect_output(
            r#"
var a = 1;
for(
  var b = 1;
  b <= 6;
  b = b + 1
) {
  a = a * b;
}
print a;"#,
            "720\n",
        )
    }

    #[test]
    fn function_return() {
        expect_output("fun square(x) { return x * x; } print square(2);", "4\n");
        expect_output(
            "fun sign(x) { if (x==0) { return 0; } if (x<0) { return -1; } return 1; } print sign(-2);",
            "-1\n",
        );
    }

    #[test]
    fn recursion() {
        expect_output(
            r#"fun factorial(x) { if(x<=1) { return 1; } return x * factorial(x-1); } print factorial(5);"#,
            "120\n",
        );
    }

    #[test]
    fn scope() {
        expect_output(
            "var a = 1; { fun get_a() { return a; } var a = 2; print get_a(); }",
            "1\n",
        )
    }
}
