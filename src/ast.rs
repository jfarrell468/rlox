use super::token::{Token, TokenType};
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
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

pub enum Expression<'a> {
    Binary {
        left: Box<Expression<'a>>,
        operator: &'a Token<'a>,
        right: Box<Expression<'a>>,
    },
    Grouping(Box<Expression<'a>>),
    Literal(&'a TokenType<'a>),
    Logical {
        left: Box<Expression<'a>>,
        operator: &'a Token<'a>,
        right: Box<Expression<'a>>,
    },
    Unary {
        operator: &'a Token<'a>,
        right: Box<Expression<'a>>,
    },
    Variable(&'a Token<'a>),
    Assign {
        name: &'a Token<'a>,
        value: Box<Expression<'a>>,
    },
}

pub trait Visitor<T, Output> {
    fn visit(&mut self, n: &T) -> Output;
}

impl<'a> Expression<'a> {
    pub fn accept<T>(&self, v: &mut Visitor<Expression<'a>, T>) -> T {
        v.visit(self)
    }
}

pub enum Statement<'a> {
    Print(Expression<'a>),
    Expression(Expression<'a>),
    Var {
        name: &'a str,
        initializer: Expression<'a>,
    },
    Block(Vec<Statement<'a>>),
    If {
        condition: Expression<'a>,
        then_branch: Box<Statement<'a>>,
        else_branch: Option<Box<Statement<'a>>>,
    },
    While {
        condition: Expression<'a>,
        body: Box<Statement<'a>>,
    },
}

impl<'a> Statement<'a> {
    pub fn accept<T>(&self, v: &mut Visitor<Statement<'a>, T>) -> T {
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
            } => self.parenthesize(operator.lexeme, vec![left, right]),
            Expression::Grouping(x) => self.parenthesize("group", vec![x]),
            Expression::Literal(x) => match x {
                TokenType::String(y) => y.to_string(),
                TokenType::Number(y) => format!("{}", y).to_string(),
                _ => String::from(""),
            },
            Expression::Unary { operator, right } => {
                self.parenthesize(operator.lexeme, vec![right])
            }
            Expression::Variable(x) => x.lexeme.to_string(),
            Expression::Assign { name, value } => {
                format!("(assign {} {})", name.lexeme, value.accept(self)).to_string()
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => self.parenthesize(operator.lexeme, vec![left, right]),
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
                operator: &Token {
                    tokentype: TokenType::Minus,
                    lexeme: "-",
                    line: 1,
                },
                right: Box::new(Expression::Literal(&TokenType::Number(123.0))),
            }),
            operator: &Token {
                tokentype: TokenType::Star,
                lexeme: "*",
                line: 1,
            },
            right: Box::new(Expression::Grouping(Box::new(Expression::Literal(
                &TokenType::Number(45.67),
            )))),
        };
        let visitor = AstPrinter {};
        assert_eq!(expression.accept(&visitor), "(* (- 123) (group 45.67))");
    }
}
