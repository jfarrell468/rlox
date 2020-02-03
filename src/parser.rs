use crate::ast::{Expression, Statement};
use crate::callable::Callable;
use crate::token::{Token, TokenType};
use std::error::Error;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
struct ParseError {
    message: String,
    prev: Token,
    cur: Token,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

macro_rules! consume {
    ($self:expr, TokenType::Identifier, $error:expr) => {
        match $self.peek()?.tokentype.clone() {
            TokenType::Identifier(name) => {
                $self.advance();
                name
            }
            _ => return Err($self.error($error)),
        }
    };
    ($self:expr, $token_type:pat, $error:expr) => {
        match $self.peek()?.tokentype {
            $token_type => $self.advance(),
            _ => return Err($self.error($error)),
        }
    };
}

macro_rules! check {
    ($self:expr, $($token_type:tt)+) => {
        if $self.is_at_end() {
            false
        } else {
            match $self.peek()?.tokentype {
                $($token_type)+ => true,
                _ => false,
            }
        }
    };
}

macro_rules! matches {
    ($self:expr, $($token_type:tt)+) => {
        if check!($self, $($token_type)+) {
            $self.advance();
            true
        } else {
            false
        }
    };
}

impl<'a> Parser<'a> {
    fn declaration(&mut self) -> Result<Statement, ParseError> {
        match self.peek()?.tokentype {
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
    fn var_declaration(&mut self) -> Result<Statement, ParseError> {
        let name = consume!(self, TokenType::Identifier, "Expect variable name.");
        consume!(
            self,
            TokenType::Equal,
            "Expect '=' after declaring variable name."
        );
        let initializer = self.expression()?;
        consume!(
            self,
            TokenType::Semicolon,
            "Expect ';' after variable declaration."
        );
        Ok(Statement::Var { name, initializer })
    }
    fn function(&mut self, kind: &str) -> Result<Statement, ParseError> {
        let name = consume!(
            self,
            TokenType::Identifier,
            format!("Expect {} name", kind).as_str()
        );
        consume!(
            self,
            TokenType::LeftParen,
            format!("Expect '(' after {} name", kind).as_str()
        );
        let mut parameters: Vec<Token> = Vec::new();
        if check!(self, TokenType::LeftParen) {
            loop {
                parameters.push(
                    consume!(self, TokenType::Identifier(_), "Expect parameter name").clone(),
                );
                if !matches!(self, TokenType::Comma) {
                    break;
                }
            }
        }
        consume!(self, TokenType::RightParen, "Expect ')' after parameters");

        consume!(self, TokenType::LeftBrace, "Expect '{{' before {} body");
        let body = self.block()?;
        Ok(Statement::Function(Rc::new(Callable {
            name: name,
            params: parameters,
            body: body,
        })))
    }
    fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek()?.tokentype {
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
    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        let keyword = self.previous().clone();
        let value = match self.peek()?.tokentype {
            TokenType::Semicolon => Expression::Literal(Token {
                tokentype: TokenType::Nil,
                lexeme: "".to_string(),
                line: 0,
            }),
            _ => self.expression()?,
        };
        consume!(self, TokenType::Semicolon, "Expect ';' after return value");
        Ok(Statement::Return { keyword, value })
    }
    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        consume!(self, TokenType::LeftParen, "Expect '(' after 'for'.");
        let initializer: Option<Statement> = match self.peek()?.tokentype {
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

        let condition = match self.peek()?.tokentype {
            TokenType::Semicolon => Expression::Literal(Token {
                tokentype: TokenType::True,
                lexeme: String::from("true"),
                line: 0,
            }),
            _ => self.expression()?,
        };
        consume!(
            self,
            TokenType::Semicolon,
            "Expect ';' after for loop condition."
        );

        let increment: Option<Expression> = match self.peek()?.tokentype {
            TokenType::RightParen => None,
            _ => Some(self.expression()?),
        };
        consume!(
            self,
            TokenType::RightParen,
            "Expect ')' after for loop clauses."
        );

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
    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        consume!(self, TokenType::LeftParen, "Expect '(' after 'while'.");
        let condition = self.expression()?;
        consume!(
            self,
            TokenType::RightParen,
            "Expect ')' after while condition."
        );
        let body = self.statement()?;
        Ok(Statement::While {
            condition: condition,
            body: Box::new(body),
        })
    }
    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        consume!(self, TokenType::LeftParen, "Expect '(' after 'if'.");
        let condition = self.expression()?;
        consume!(
            self,
            TokenType::RightParen,
            "Expect ')' after if condition."
        );
        let then_branch = self.statement()?;
        match self.peek()?.tokentype {
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
    fn block(&mut self) -> Result<Statement, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() {
            if let TokenType::RightBrace = self.peek()?.tokentype {
                break;
            }
            statements.push(self.declaration()?);
        }
        consume!(self, TokenType::RightBrace, "Expect '}' after block");
        Ok(Statement::Block(statements))
    }
    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        consume!(self, TokenType::Semicolon, "Expect ';' after value.");
        Ok(Statement::Print(expr))
    }
    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        consume!(self, TokenType::Semicolon, "Expect ';' after expression.");
        Ok(Statement::Expression(expr))
    }
    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.or()?;
        match self.peek()?.tokentype {
            TokenType::Equal => {
                self.advance();
                //let equals = self.previous();
                let value = self.assignment()?;
                match expr {
                    Expression::Variable(x) => Ok(Expression::Assign {
                        name: x,
                        value: Box::new(value),
                    }),
                    _ => Err(self.error("Invalid assignment target.")),
                }
            }
            _ => Ok(expr),
        }
    }
    fn or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.and()?;
        while matches!(self, TokenType::Or) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expression::Logical {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.equality()?;
        while matches!(self, TokenType::And) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expression::Logical {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;
        while matches!(self, TokenType::BangEqual | TokenType::EqualEqual) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.addition()?;
        while matches!(
            self,
            TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual
        ) {
            let operator = self.previous().clone();
            let right = self.addition()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn addition(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.multiplication()?;
        while matches!(self, TokenType::Minus | TokenType::Plus) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn multiplication(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary()?;
        while matches!(self, TokenType::Slash | TokenType::Star) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expression, ParseError> {
        match self.peek()?.tokentype {
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
    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.primary()?;
        loop {
            match self.peek()?.tokentype {
                TokenType::LeftParen => {
                    self.advance();
                    expr = self.finish_call(expr)?;
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        let mut arguments: Vec<Expression> = Vec::new();
        match self.peek()?.tokentype {
            TokenType::RightParen => (),
            _ => {
                loop {
                    // TODO: Restrict max args to 255.
                    arguments.push(self.expression()?);
                    match self.peek()?.tokentype {
                        TokenType::Comma => self.advance(),
                        _ => break,
                    };
                }
            }
        }
        let paren = consume!(
            self,
            TokenType::RightParen,
            "Expect ')' after function arguments."
        )
        .clone();
        Ok(Expression::Call {
            callee: Box::new(callee),
            paren: paren,
            arguments: arguments,
        })
    }
    fn primary(&mut self) -> Result<Expression, ParseError> {
        match self.peek()?.tokentype {
            TokenType::False
            | TokenType::True
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => Ok(Expression::Literal(self.advance().clone())),
            TokenType::Identifier(_) => Ok(Expression::Variable(self.advance().clone())),
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                consume!(self, TokenType::RightParen, "Expect ')' after expression.");
                Ok(Expression::Grouping(Box::new(expr)))
            }
            _ => Err(self.error("Expect expression.")),
        }
    }
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if let TokenType::Semicolon = self.previous().tokentype {
                return;
            }
            match self.peek().unwrap().tokentype {
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
        match self.peek().unwrap().tokentype {
            TokenType::EOF => true,
            _ => false,
        }
    }
    fn peek(&self) -> Result<&Token, ParseError> {
        match self.tokens.get(self.current) {
            None => Err(self.error("No more tokens")),
            Some(x) => Ok(x),
        }
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
            cur: self.peek().unwrap().clone(),
        }
    }
}

#[cfg(test)]
mod parse_error_tests {
    use crate::scanner;
    use crate::parser;

    fn expect_error(source: &str, expected_description: &str) {
        let (tokens, success) = scanner::scan_tokens(source);
        assert!(success);
        let result = parser::parse(&tokens);
        assert!(!result.is_ok());
        assert_eq!(result.err().unwrap().description(), expected_description)
    }

    #[test]
    fn basic_test() {
        expect_error("var 1;", "Expect variable name.")
    }
}