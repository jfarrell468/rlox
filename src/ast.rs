use crate::callable::Callable;
use crate::environment::Environment;
use crate::token::{Token, TokenType};
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable, Environment),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Callable(x, _) => write!(f, "{}", x),
        }
    }
}
impl Value {
    pub fn type_str(&self) -> &str {
        match self {
            Value::Nil => "nil",
            Value::Boolean(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Callable(_, _) => "callable",
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Literal(Token),
    Logical {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        right: Box<Expression>,
    },
    Variable {
        name: Token,
        scope: Option<usize>,
    },
    Assign {
        name: Token,
        value: Box<Expression>,
        scope: Option<usize>,
    },
    Call {
        callee: Box<Expression>,
        paren: Token,
        arguments: Vec<Expression>,
    },
}

pub trait Visitor<T, Output> {
    fn visit(&mut self, n: &T) -> Output;
}

pub trait MutatingVisitor<T, Output> {
    fn visit(&mut self, n: &mut T) -> Output;
}

impl Expression {
    pub fn accept<T>(&self, v: &mut Visitor<Expression, T>) -> T {
        v.visit(self)
    }
    pub fn accept_mut<T>(&mut self, v: &mut MutatingVisitor<Expression, T>) -> T {
        v.visit(self)
    }
}

#[derive(Debug)]
pub enum Statement {
    Print(Expression),
    Expression(Expression),
    Var {
        name: Token,
        initializer: Expression,
    },
    Block(Vec<Statement>),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    Function(Callable),
    Return {
        keyword: Token,
        value: Expression,
    },
}

impl Statement {
    pub fn accept<T>(&self, v: &mut Visitor<Statement, T>) -> T {
        v.visit(self)
    }
    pub fn accept_mut<T>(&mut self, v: &mut MutatingVisitor<Statement, T>) -> T {
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

impl Visitor<Expression, String> for AstPrinter {
    fn visit(&mut self, n: &Expression) -> String {
        match n {
            Expression::Binary {
                left,
                operator,
                right,
            } => self.parenthesize(&operator.lexeme, vec![left, right]),
            Expression::Grouping(x) => self.parenthesize("group", vec![x]),
            Expression::Literal(x) => match &x.tokentype {
                TokenType::String(y) => y.clone(),
                TokenType::Number(y) => format!("{}", y),
                _ => String::from(""),
            },
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
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use crate::ast::{AstPrinter, Expression};
    use crate::token::{Token, TokenType};

    #[test]
    fn basic_ast_test() {
        let expression = Expression::Binary {
            left: Box::new(Expression::Unary {
                operator: Token {
                    tokentype: TokenType::Minus,
                    lexeme: String::from("-"),
                    line: 1,
                },
                right: Box::new(Expression::Literal(Token {
                    tokentype: TokenType::Number(123.0),
                    lexeme: String::from("123.0"),
                    line: 1,
                })),
            }),
            operator: Token {
                tokentype: TokenType::Star,
                lexeme: String::from("*"),
                line: 1,
            },
            right: Box::new(Expression::Grouping(Box::new(Expression::Literal(Token {
                tokentype: TokenType::Number(45.67),
                lexeme: String::from("45.67"),
                line: 1,
            })))),
        };
        let mut visitor = AstPrinter {};
        assert_eq!(expression.accept(&mut visitor), "(* (- 123) (group 45.67))");
    }
}
