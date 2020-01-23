use super::ast::{Expression, Statement};
use super::token::{Token, TokenType, TRUE_TOKEN};
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
            "[line {}] Parse Error: {}\n  Context: {} {}",
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
    pub fn parse(&mut self) -> Result<Vec<Statement<'a>>, Box<dyn Error + 'a>> {
        let mut statements: Vec<Statement<'a>> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }
    fn declaration(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::Var => {
                self.advance();
                self.var_declaration()
            }
            _ => self.statement(),
        }
    }
    fn var_declaration(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        let name = &self.peek().tokentype;
        match name {
            TokenType::Identifier(name) => {
                self.advance();
                match self.peek().tokentype {
                    TokenType::Equal => {
                        self.advance();
                        let initializer = self.expression()?;
                        match self.peek().tokentype {
                            TokenType::Semicolon => {
                                self.advance();
                                Ok(Statement::Var {
                                    name: name,
                                    initializer: initializer,
                                })
                            }
                            _ => Err(Box::new(
                                self.error("Expect ';' after variable declaration."),
                            )),
                        }
                    }
                    _ => Err(Box::new(
                        self.error("Expect '=' after declaring variable name."),
                    )),
                }
            }
            _ => Err(Box::new(self.error("Expect variable name."))),
        }
    }
    fn statement(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::If => {
                self.advance();
                self.if_statement()
            }
            TokenType::Print => {
                self.advance();
                self.print_statement()
            }
            TokenType::LeftBrace => {
                self.advance();
                self.block()
            }
            TokenType::While => {
                self.advance();
                self.while_statement()
            }
            TokenType::For => {
                self.advance();
                self.for_statement()
            }
            _ => self.expression_statement(),
        }
    }
    fn for_statement(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::LeftParen => {
                self.advance();
                let initializer: Option<Statement> = match self.peek().tokentype {
                    TokenType::Semicolon => {
                        self.advance();
                        None
                    }
                    TokenType::Var => {
                        self.advance();
                        Some(self.var_declaration()?)
                    }
                    _ => Some(self.expression_statement()?),
                };

                let condition = match self.peek().tokentype {
                    TokenType::Semicolon => Expression::Literal(&TRUE_TOKEN),
                    _ => self.expression()?,
                };
                match self.peek().tokentype {
                    TokenType::Semicolon => {
                        self.advance();
                    }
                    _ => {
                        return Err(Box::new(self.error("Expect ';' after for loop condition.")));
                    }
                }

                let increment: Option<Expression> = match self.peek().tokentype {
                    TokenType::RightParen => None,
                    _ => Some(self.expression()?),
                };
                match self.peek().tokentype {
                    TokenType::RightParen => {
                        self.advance();
                    }
                    _ => {
                        return Err(Box::new(self.error("Expect ')' after for loop clauses.")));
                    }
                }

                let mut body = self.statement()?;

                if let Some(x) = increment {
                    body = Statement::Block(vec![body, Statement::Expression(x)])
                }
                body = Statement::While {
                    condition: condition,
                    body: Box::new(body),
                };
                match initializer {
                    None => Ok(body),
                    Some(x) => Ok(Statement::Block(vec![x, body])),
                }
            }
            _ => Err(Box::new(self.error("Expect '(' after 'for'."))),
        }
    }
    fn while_statement(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::LeftParen => {
                self.advance();
                let condition = self.expression()?;
                match self.peek().tokentype {
                    TokenType::RightParen => {
                        self.advance();
                        let body = self.statement()?;
                        Ok(Statement::While {
                            condition: condition,
                            body: Box::new(body),
                        })
                    }
                    _ => Err(Box::new(self.error("Expect ')' after if condition."))),
                }
            }
            _ => Err(Box::new(self.error("Expect '(' after 'if'."))),
        }
    }
    fn if_statement(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        match self.peek().tokentype {
            TokenType::LeftParen => {
                self.advance();
                let condition = self.expression()?;
                match self.peek().tokentype {
                    TokenType::RightParen => {
                        self.advance();
                        let then_branch = self.statement()?;
                        match self.peek().tokentype {
                            TokenType::Else => {
                                self.advance();
                                let else_branch = self.statement()?;
                                Ok(Statement::If {
                                    condition: condition,
                                    then_branch: Box::new(then_branch),
                                    else_branch: Some(Box::new(else_branch)),
                                })
                            }
                            _ => Ok(Statement::If {
                                condition: condition,
                                then_branch: Box::new(then_branch),
                                else_branch: None,
                            }),
                        }
                    }
                    _ => Err(Box::new(self.error("Expect ')' after if condition."))),
                }
            }
            _ => Err(Box::new(self.error("Expect '(' after 'if'."))),
        }
    }
    fn block(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        let mut statements: Vec<Statement<'a>> = Vec::new();
        while !self.is_at_end() {
            if let TokenType::RightBrace = self.peek().tokentype {
                break;
            }
            statements.push(self.declaration()?);
        }
        match self.peek().tokentype {
            TokenType::RightBrace => {
                self.advance();
                Ok(Statement::Block(statements))
            }
            _ => Err(Box::new(self.error("Expect '}' after block"))),
        }
    }
    fn print_statement(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        let expr = self.expression()?;
        match self.peek().tokentype {
            TokenType::Semicolon => {
                self.advance();
                Ok(Statement::Print(expr))
            }
            _ => Err(Box::new(self.error("Expect ';' after value."))),
        }
    }
    fn expression_statement(&mut self) -> Result<Statement<'a>, Box<dyn Error + 'a>> {
        let expr = self.expression()?;
        match self.peek().tokentype {
            TokenType::Semicolon => {
                self.advance();
                Ok(Statement::Expression(expr))
            }
            _ => Err(Box::new(self.error("Expect ';' after expression."))),
        }
    }
    fn expression(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let expr = self.or()?;
        match self.peek().tokentype {
            TokenType::Equal => {
                self.advance();
                //let equals = self.previous();
                let value = self.assignment()?;
                match expr {
                    Expression::Variable(x) => Ok(Expression::Assign {
                        name: x,
                        value: Box::new(value),
                    }),
                    _ => Err(Box::new(self.error("Invalid assignment target."))),
                }
            }
            _ => Ok(expr),
        }
    }
    fn or(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let mut expr = self.and()?;
        loop {
            match self.peek().tokentype {
                TokenType::Or => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.and()?;
                    expr = Expression::Logical {
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
    fn and(&mut self) -> Result<Expression<'a>, Box<dyn Error + 'a>> {
        let mut expr = self.equality()?;
        loop {
            match self.peek().tokentype {
                TokenType::And => {
                    self.advance();
                    let operator = self.previous();
                    let right = self.equality()?;
                    expr = Expression::Logical {
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
            TokenType::Identifier(_) => {
                self.advance();
                Ok(Expression::Variable(self.previous()))
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
