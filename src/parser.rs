use crate::ast::{Expression, Statement};
use crate::callable::Callable;
use crate::token::{Token, TokenType};
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Debug)]
struct ParseError {
    message: String,
    prev: Token,
    cur: Token,
}

impl fmt::Display for ParseError {
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

impl Error for ParseError {
    fn description(&self) -> &str {
        &self.message
    }
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Statement>, Box<dyn Error>> {
    let mut parser = Parser {
        tokens: tokens,
        current: 0,
    };
    let mut statements: Vec<Statement> = Vec::new();
    while !parser.is_at_end() {
        statements.push(parser.declaration()?);
    }
    Ok(statements)
}

impl<'a> Parser<'a> {
    fn declaration(&mut self) -> Result<Statement, Box<dyn Error>> {
        match self.peek().tokentype {
            TokenType::Fun => {
                self.advance();
                self.function("function")
            }
            TokenType::Var => {
                self.advance();
                self.var_declaration()
            }
            _ => self.statement(),
        }
    }
    fn var_declaration(&mut self) -> Result<Statement, Box<dyn Error>> {
        let name = self.peek().tokentype.clone();
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
                                    name: name.clone(),
                                    initializer,
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
    fn function(&mut self, kind: &str) -> Result<Statement, Box<dyn Error>> {
        match self.peek().tokentype.clone() {
            TokenType::Identifier(name) => {
                self.advance();
                match self.peek().tokentype {
                    TokenType::LeftParen => {
                        self.advance();
                        let mut parameters: Vec<Token> = Vec::new();
                        match self.peek().tokentype {
                            TokenType::RightParen => (),
                            _ => loop {
                                match self.peek().tokentype.clone() {
                                    TokenType::Identifier(x) => {
                                        parameters.push(self.advance().clone());
                                    }
                                    _ => return Err(Box::new(self.error("Expect parameter name"))),
                                }
                                match self.peek().tokentype {
                                    TokenType::Comma => self.advance(),
                                    _ => break,
                                };
                            },
                        }
                        match self.peek().tokentype {
                            TokenType::RightParen => {
                                self.advance();
                                match self.peek().tokentype {
                                    TokenType::LeftBrace => {
                                        self.advance();
                                        let body = self.block()?;
                                        Ok(Statement::Function(Rc::new(Callable {
                                            name: name.clone(),
                                            params: parameters,
                                            body: body,
                                        })))
                                    }
                                    _ => Err(Box::new(self.error(
                                        format!("Expect '{{' before {} body", kind).as_str(),
                                    ))),
                                }
                            }
                            _ => Err(Box::new(self.error("Expect ')' after parameters"))),
                        }
                    }
                    _ => Err(Box::new(
                        self.error(format!("Expect '(' after {} name", kind).as_str()),
                    )),
                }
            }
            _ => Err(Box::new(
                self.error(format!("Expect {} name", kind).as_str()),
            )),
        }
    }
    fn statement(&mut self) -> Result<Statement, Box<dyn Error>> {
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
            TokenType::Return => {
                self.advance();
                self.return_statement()
            }
            _ => self.expression_statement(),
        }
    }
    fn return_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let keyword = self.previous().clone();
        let expr = match self.peek().tokentype {
            TokenType::Semicolon => Expression::Literal(Token {
                tokentype: TokenType::Nil,
                lexeme: "".to_string(),
                line: 0,
            }),
            _ => self.expression()?,
        };
        match self.peek().tokentype {
            TokenType::Semicolon => {
                self.advance();
                Ok(Statement::Return {
                    keyword: keyword,
                    value: expr,
                })
            }
            _ => Err(Box::new(self.error("Expect ';' after return value"))),
        }
    }
    fn for_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
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
                    TokenType::Semicolon => Expression::Literal(Token {
                        tokentype: TokenType::True,
                        lexeme: String::from("true"),
                        line: 0,
                    }),
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
    fn while_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
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
    fn if_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
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
    fn block(&mut self) -> Result<Statement, Box<dyn Error>> {
        let mut statements: Vec<Statement> = Vec::new();
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
    fn print_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let expr = self.expression()?;
        match self.peek().tokentype {
            TokenType::Semicolon => {
                self.advance();
                Ok(Statement::Print(expr))
            }
            _ => Err(Box::new(self.error("Expect ';' after value."))),
        }
    }
    fn expression_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let expr = self.expression()?;
        match self.peek().tokentype {
            TokenType::Semicolon => {
                self.advance();
                Ok(Statement::Expression(expr))
            }
            _ => Err(Box::new(self.error("Expect ';' after expression."))),
        }
    }
    fn expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expression, Box<dyn Error>> {
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
    fn or(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.and()?;
        loop {
            match self.peek().tokentype.clone() {
                TokenType::Or => {
                    let operator = self.advance().clone();
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
    fn and(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.equality()?;
        loop {
            match self.peek().tokentype {
                TokenType::And => {
                    let operator = self.advance().clone();
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
    fn equality(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.comparison()?;
        loop {
            match self.peek().tokentype {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let operator = self.advance().clone();
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
    fn comparison(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.addition()?;
        loop {
            match self.peek().tokentype {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    let operator = self.advance().clone();
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
    fn addition(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.multiplication()?;
        loop {
            match self.peek().tokentype {
                TokenType::Minus | TokenType::Plus => {
                    let operator = self.advance().clone();
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
    fn multiplication(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.unary()?;
        loop {
            match self.peek().tokentype {
                TokenType::Slash | TokenType::Star => {
                    let operator = self.advance().clone();
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
    fn unary(&mut self) -> Result<Expression, Box<dyn Error>> {
        match self.peek().tokentype {
            TokenType::Bang | TokenType::Minus => {
                let operator = self.advance().clone();
                let right = self.unary()?;
                Ok(Expression::Unary {
                    operator: operator,
                    right: Box::new(right),
                })
            }
            _ => self.call(),
        }
    }
    fn call(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.primary()?;
        loop {
            match self.peek().tokentype {
                TokenType::LeftParen => {
                    self.advance();
                    expr = self.finish_call(expr)?;
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn finish_call(&mut self, callee: Expression) -> Result<Expression, Box<dyn Error>> {
        let mut arguments: Vec<Expression> = Vec::new();
        match self.peek().tokentype {
            TokenType::RightParen => (),
            _ => {
                loop {
                    // TODO: Restrict max args to 255.
                    arguments.push(self.expression()?);
                    match self.peek().tokentype {
                        TokenType::Comma => self.advance(),
                        _ => break,
                    };
                }
            }
        }
        let paren = self.peek().clone();
        match paren.tokentype {
            TokenType::RightParen => {
                self.advance();
                Ok(Expression::Call {
                    callee: Box::new(callee),
                    paren: paren,
                    arguments: arguments,
                })
            }
            _ => Err(Box::new(self.error("Expect ')' after function arguments."))),
        }
    }
    fn primary(&mut self) -> Result<Expression, Box<dyn Error>> {
        match self.peek().tokentype {
            TokenType::False
            | TokenType::True
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => Ok(Expression::Literal(self.advance().clone())),
            TokenType::Identifier(_) => Ok(Expression::Variable(self.advance().clone())),
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
    fn advance(&mut self) -> &Token {
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
    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }
    fn previous(&self) -> &Token {
        self.tokens
            .get(if self.current > 0 {
                self.current - 1
            } else {
                0
            })
            .expect("Failed to get previous")
    }
    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            prev: self.previous().clone(),
            cur: self.peek().clone(),
        }
    }
}
