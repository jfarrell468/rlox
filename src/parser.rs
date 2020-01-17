use super::ast::Expression;
use super::token::{Token, TokenType};
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)]
struct ParseError<'a> {
    message: String,
    prev: &'a Token<'a>,
    cur: &'a Token<'a>,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] Error: {}\n  Context: {} {}",
            self.cur.line,
            self.message.as_str(),
            self.prev.lexeme,
            self.cur.lexeme
        )
    }
}

impl<'a> Error for ParseError<'a> {
    fn description(&self) -> &str {
        &self.message
    }
}

pub struct Parser<'a> {
    tokens: &'a Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Parser {
        Parser {
            tokens: tokens,
            current: 0,
        }
    }
    pub fn parse(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        self.expression()
    }
    fn expression(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        self.equality()
    }
    fn equality(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let mut expr = self.comparison()?;
        loop {
            match self.peek().tokentype {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.comparison()?;
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let mut expr = self.addition()?;
        loop {
            match self.peek().tokentype {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.addition()?;
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn addition(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let mut expr = self.multiplication()?;
        loop {
            match self.peek().tokentype {
                TokenType::Minus | TokenType::Plus => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.multiplication()?;
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn multiplication(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let mut expr = self.unary()?;
        loop {
            match self.peek().tokentype {
                TokenType::Slash | TokenType::Star => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.unary()?;
                    expr = Expression::Binary {
                        left: Box::new(expr),
                        operator: operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::Bang | TokenType::Minus => {
                self.advance();
                let operator = self.previous();
                let right = self.unary()?;
                Ok(Expression::Unary {
                    operator: operator,
                    right: Box::new(right),
                })
            }
            _ => self.primary(),
        }
    }
    fn primary(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::False
            | TokenType::True
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => {
                self.advance();
                Ok(Expression::Literal(&self.previous().tokentype))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                match self.peek().tokentype {
                    TokenType::RightParen => {
                        self.advance();
                        Ok(Expression::Grouping(Box::new(expr)))
                    }
                    _ => Err(Box::new(self.error("Expect ')' after expression."))),
                }
            }
            _ => Err(Box::new(self.error("Expect expression."))),
        }
    }
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if let TokenType::Semicolon = self.previous().tokentype {
                return;
            }
            match self.peek().tokentype {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
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
            _ => false,
        }
    }
    fn peek(&self) -> &'a Token<'a> {
        self.tokens.get(self.current).unwrap()
    }
    fn previous(&self) -> &'a Token<'a> {
        self.tokens
            .get(if self.current > 0 {
                self.current - 1
            } else {
                0
            })
            .expect("Failed to get previous")
    }
    fn error(&self, msg: &str) -> ParseError<'a> {
        ParseError {
            message: msg.to_string(),
            prev: self.previous(),
            cur: self.peek(),
        }
    }
}
