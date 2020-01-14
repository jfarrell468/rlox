use super::token::{Token, TokenType};

enum Expression<'a> {
    Binary {
        left: Box<Expression<'a>>,
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Grouping(Box<Expression<'a>>),
    Literal(TokenType<'a>),
    Unary {
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
}

trait ExprVisitor<T> {
    fn visit(&self, n: &Expression) -> T;
}

impl<'a> Expression<'a> {
    fn accept<T>(&self, v: &ExprVisitor<T>) -> T {
        v.visit(self)
    }
}

#[cfg(test)]
mod ast_tests {
    use crate::ast::ExprVisitor;
    use crate::ast::Expression;
    use crate::token::{Token, TokenType};

    struct AstPrinter {}
    impl AstPrinter {
        fn parenthesize(&self, name: &str, args: Vec<&Expression>) -> String {
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
    impl ExprVisitor<String> for AstPrinter {
        fn visit(&self, n: &Expression) -> String {
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
            }
        }
    }

    #[test]
    fn basic_ast_test() {
        let expression = Expression::Binary {
            left: Box::new(Expression::Unary {
                operator: Token {
                    tokentype: TokenType::Minus,
                    lexeme: "-",
                    line: 1,
                },
                right: Box::new(Expression::Literal(TokenType::Number(123.0))),
            }),
            operator: Token {
                tokentype: TokenType::Star,
                lexeme: "*",
                line: 1,
            },
            right: Box::new(Expression::Grouping(Box::new(Expression::Literal(
                TokenType::Number(45.67),
            )))),
        };
        let visitor = AstPrinter {};
        assert_eq!(expression.accept(&visitor), "(* (- 123) (group 45.67))");
    }
}
