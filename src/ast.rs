use crate::callable::Callable;
use crate::token::{Token, TokenType};
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Rc<Callable>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Callable(x) => write!(f, "{}", x),
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
    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expression>,
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

impl Expression {
    pub fn accept<T>(&self, v: &mut Visitor<Expression, T>) -> T {
        v.visit(self)
    }
}

#[derive(Debug)]
pub enum Statement {
    Print(Expression),
    Expression(Expression),
    Var {
        name: String,
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
    Function(Rc<Callable>),
    Return {
        keyword: Token,
        value: Expression,
    },
}

impl Statement {
    pub fn accept<T>(&self, v: &mut Visitor<Statement, T>) -> T {
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
            Expression::Variable(x) => x.lexeme.to_string(),
            Expression::Assign { name, value } => {
                format!("(assign {} {})", name.lexeme, value.accept(self)).to_string()
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => self.parenthesize(&operator.lexeme, vec![left, right]),
            Expression::Call {
                callee,
                paren,
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
        let visitor = AstPrinter {};
        assert_eq!(expression.accept(&visitor), "(* (- 123) (group 45.67))");
    }
}
