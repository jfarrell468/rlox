use super::ast::Expression;
use super::token::{Token, TokenType};

pub struct Parser<'a> {
    tokens: &'a Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: &'a Vec<Token<'a>>) -> Expression<'a> {
        let mut parser = Parser {
            tokens: tokens,
            current: 0,
        };
        parser.expression()
    }
    fn expression(&mut self) -> Expression<'a> {
        self.equality()
    }
    fn equality(&mut self) -> Expression<'a> {
        let mut expr = self.comparison();
        loop {
            match self.peek().tokentype {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.comparison();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }
    fn comparison(&mut self) -> Expression<'a> {
        let mut expr = self.addition();
        loop {
            match self.peek().tokentype {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.addition();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }
    fn addition(&mut self) -> Expression<'a> {
        let mut expr = self.multiplication();
        loop {
            match self.peek().tokentype {
                TokenType::Minus | TokenType::Plus => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.multiplication();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }
    fn multiplication(&mut self) -> Expression<'a> {
        let mut expr = self.unary();
        loop {
            match self.peek().tokentype {
                TokenType::Slash | TokenType::Star => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.unary();
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        expr
    }
    fn unary(&mut self) -> Expression<'a> {
        match self.peek().tokentype {
            TokenType::Bang | TokenType::Minus => {
                self.advance();
                println!("Match: {:?}, {:?}", self.previous(), self.peek());
                let operator = self.previous();
                let right = self.unary();
                Expression::Unary {
                    operator: operator,
                    right: Box::new(right),
                }
            }
            _ => self.primary(),
        }
    }
    fn primary(&mut self) -> Expression<'a> {
        match self.peek().tokentype {
            TokenType::False
            | TokenType::True
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => {
                self.advance();
                Expression::Literal(&self.previous().tokentype)
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression();
                match self.peek().tokentype {
                    TokenType::RightParen => {
                        self.advance();
                    }
                    _ => {
                        panic!("Expect ')' after expression.");
                    }
                }
                Expression::Grouping(Box::new(expr))
            }
            _ => panic!(
                "Failed to parse expression: {:?}, {:?}",
                self.previous(),
                self.peek()
            ),
        }
    }
    fn advance(&mut self) -> &'a Token<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn is_at_end(&self) -> bool {
        match self.peek().tokentype {
            TokenType::EOF => true,
            _ => false
        }
    }
    fn peek(&self) -> &'a Token<'a> {
        self.tokens.get(self.current).expect("Failed to peek")
    }
    fn previous(&self) -> &'a Token<'a> {
        self.tokens
            .get(self.current - 1)
            .expect("Failed to get previous")
    }
}
