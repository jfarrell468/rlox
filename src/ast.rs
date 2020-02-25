use crate::callable::{LoxFunction, NativeFunction};
use crate::class::Class;
use crate::environment::Environment;
use crate::instance::Instance;
use crate::token::Token;
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Function(LoxFunction<'a>, Environment<'a>, bool),
    NativeFunction(NativeFunction<'a>),
    Class(Class<'a>),
    Instance(Instance<'a>),
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
            Value::Function(x, _, _) => write!(f, "{}", x),
            Value::NativeFunction(x) => write!(f, "{}", x),
            Value::Class(x) => write!(f, "{}", x),
            Value::Instance(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    Binary {
        left: Box<Expression<'a>>,
        operator: &'a Token<'a>,
        right: Box<Expression<'a>>,
    },
    Grouping(Box<Expression<'a>>),
    Literal(&'a Token<'a>),
    Logical {
        left: Box<Expression<'a>>,
        operator: &'a Token<'a>,
        right: Box<Expression<'a>>,
    },
    Unary {
        operator: &'a Token<'a>,
        right: Box<Expression<'a>>,
    },
    Variable {
        name: &'a Token<'a>,
        scope: Option<usize>,
    },
    Assign {
        name: &'a Token<'a>,
        value: Box<Expression<'a>>,
        scope: Option<usize>,
    },
    Call {
        callee: Box<Expression<'a>>,
        paren: &'a Token<'a>,
        arguments: Vec<Expression<'a>>,
    },
    Get {
        object: Box<Expression<'a>>,
        name: &'a Token<'a>,
    },
    Set {
        object: Box<Expression<'a>>,
        name: &'a Token<'a>,
        value: Box<Expression<'a>>,
    },
    This {
        token: &'a Token<'a>,
        scope: Option<usize>,
    },
    Super {
        keyword: &'a Token<'a>,
        method: &'a Token<'a>,
        scope: Option<usize>,
    },
}

pub trait Visitor<T, Output> {
    fn visit(&mut self, n: &T) -> Output;
}

pub trait MutatingVisitor<T, Output> {
    fn visit(&mut self, n: &mut T) -> Output;
}

impl<'a> Expression<'a> {
    pub fn accept<T>(&self, v: &mut dyn Visitor<Expression<'a>, T>) -> T {
        v.visit(self)
    }
    pub fn accept_mut<T>(&mut self, v: &mut dyn MutatingVisitor<Expression<'a>, T>) -> T {
        v.visit(self)
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    Print(Expression<'a>),
    Expression(Expression<'a>),
    Var {
        name: &'a Token<'a>,
        initializer: Option<Expression<'a>>,
    },
    Block(Vec<Statement<'a>>),
    If {
        condition: Expression<'a>,
        then_branch: Box<Statement<'a>>,
        else_branch: Option<Box<Statement<'a>>>,
    },
    While {
        condition: Option<Expression<'a>>,
        body: Box<Statement<'a>>,
    },
    Function(LoxFunction<'a>),
    Return {
        keyword: &'a Token<'a>,
        value: Option<Expression<'a>>,
    },
    Class {
        name: &'a Token<'a>,
        superclass: Option<Expression<'a>>,
        methods: Vec<Statement<'a>>,
    },
}

impl<'a> Statement<'a> {
    pub fn accept<T>(&self, v: &mut dyn Visitor<Statement<'a>, T>) -> T {
        v.visit(self)
    }
    pub fn accept_mut<T>(&mut self, v: &mut dyn MutatingVisitor<Statement<'a>, T>) -> T {
        v.visit(self)
    }
}

pub struct AstPrinter {}

impl AstPrinter {
    fn parenthesize(&mut self, name: &str, args: Vec<&Expression>) -> String {
        let mut x = String::from("(");
        x.push_str(name);
        for arg in args {
            x.push_str(" ");
            x.push_str(arg.accept(self).as_str());
        }
        x.push_str(")");
        x
    }
}

impl<'a> Visitor<Expression<'a>, String> for AstPrinter {
    fn visit(&mut self, n: &Expression) -> String {
        match n {
            Expression::Binary {
                left,
                operator,
                right,
            } => self.parenthesize(&operator.lexeme, vec![left, right]),
            Expression::Grouping(x) => self.parenthesize("group", vec![x]),
            Expression::Literal(x) => x.lexeme.to_string(),
            Expression::Unary { operator, right } => {
                self.parenthesize(&operator.lexeme, vec![right])
            }
            Expression::Variable { name, scope: _ } => name.lexeme.to_string(),
            Expression::Assign {
                name,
                value,
                scope: _,
            } => format!("(assign {} {})", name.lexeme, value.accept(self)).to_string(),
            Expression::Logical {
                left,
                operator,
                right,
            } => self.parenthesize(&operator.lexeme, vec![left, right]),
            Expression::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let mut foo: Vec<&Expression> = Vec::new();
                for bar in arguments {
                    foo.push(bar)
                }
                let baz = self.parenthesize("call", vec![callee]);
                self.parenthesize(baz.as_str(), foo)
            }
            Expression::Get { object: _, name: _ } => String::from("(get)"),
            Expression::Set {
                object: _,
                name: _,
                value: _,
            } => String::from("(set)"),
            Expression::This { token: _, scope: _ } => String::from("this"),
            Expression::Super {
                keyword: _,
                method: _,
                scope: _,
            } => String::from("super"),
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use crate::ast::{AstPrinter, Expression};
    use crate::token::{Token, TokenType};

    #[test]
    fn basic_ast_test() {
        let minus = Token {
            tokentype: TokenType::Minus,
            lexeme: "-",
            line: 1,
        };
        let star = Token {
            tokentype: TokenType::Star,
            lexeme: "*",
            line: 1,
        };
        let number = Token {
            tokentype: TokenType::Number,
            lexeme: "123",
            line: 1,
        };
        let number2 = Token {
            tokentype: TokenType::Number,
            lexeme: "45.67",
            line: 1,
        };
        let expression = Expression::Binary {
            left: Box::new(Expression::Unary {
                operator: &minus,
                right: Box::new(Expression::Literal(&number)),
            }),
            operator: &star,
            right: Box::new(Expression::Grouping(Box::new(Expression::Literal(
                &number2,
            )))),
        };
        let mut visitor = AstPrinter {};
        assert_eq!(expression.accept(&mut visitor), "(* (- 123) (group 45.67))");
    }
}
