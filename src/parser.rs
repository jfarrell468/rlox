use crate::ast::{Expression, Statement};
use crate::callable::LoxFunction;
use crate::token::{Token, TokenType};
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ParseError<'a> {
    message: String,
    cur: &'a Token,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] Error at {}: {}",
            self.cur.line,
            match self.cur.tokentype {
                TokenType::EOF => "end".to_string(),
                _ => format!("'{}'", self.cur.lexeme),
            }
            .as_str(),
            self.message.as_str(),
        )
    }
}

impl<'a> Error for ParseError<'a> {
    fn description(&self) -> &str {
        &self.message
    }
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

pub fn parse<'a>(tokens: &'a Vec<Token>) -> (Vec<Statement<'a>>, Option<ParseError>) {
    let mut parser = Parser {
        tokens: tokens,
        current: 0,
    };
    let mut statements: Vec<Statement> = Vec::new();
    let mut last_err: Option<ParseError> = None;
    while !parser.is_at_end() {
        match parser.declaration() {
            Ok(stmt) => statements.push(stmt),
            Err(err) => {
                eprintln!("{}", err);
                last_err = Some(err);
            }
        }
    }
    (statements, last_err)
}

macro_rules! consume {
    ($self:expr, TokenType::Identifier, $error:expr) => {
        match $self.peek()?.tokentype.clone() {
            TokenType::Identifier(_) => $self.advance(),
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
    fn declaration(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        match self.peek()?.tokentype {
            TokenType::Fun => {
                self.advance();
                self.function("function")
            }
            TokenType::Var => {
                self.advance();
                self.var_declaration()
            }
            TokenType::Class => {
                self.advance();
                self.class_declaration()
            }
            _ => self.statement(),
        }
        .or_else(|err| {
            self.synchronize();
            Err(err)
        })
    }
    fn class_declaration(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let name = consume!(self, TokenType::Identifier, "Expect class name.");
        consume!(self, TokenType::LeftBrace, "Expect '{' before class body.");
        let mut methods: Vec<Statement> = Vec::new();
        while !check!(self, TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        consume!(self, TokenType::RightBrace, "Expect '}' after class body.");
        Ok(Statement::Class { name, methods })
    }
    fn var_declaration(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let name = consume!(self, TokenType::Identifier, "Expect variable name.");

        let initializer = if matches!(self, TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        consume!(
            self,
            TokenType::Semicolon,
            "Expect ';' after variable declaration."
        );
        Ok(Statement::Var {
            name: name,
            initializer,
        })
    }
    fn function(&mut self, kind: &str) -> Result<Statement<'a>, ParseError<'a>> {
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
        let mut parameters: Vec<&Token> = Vec::new();
        if !check!(self, TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    return Err(self.error("Cannot have more than 255 parameters."));
                }
                parameters
                    .push(consume!(self, TokenType::Identifier, "Expect parameter name."));
                if !matches!(self, TokenType::Comma) {
                    break;
                }
            }
        }
        consume!(self, TokenType::RightParen, "Expect ')' after parameters.");

        consume!(
            self,
            TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind)
        );
        let body = self.block()?;
        Ok(Statement::Function(LoxFunction::new(
            name,
            parameters,
            body,
        )))
    }
    fn statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
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
                Ok(Statement::Block(self.block()?))
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
    fn return_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let keyword = self.previous();
        let value = match self.peek()?.tokentype {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };
        consume!(self, TokenType::Semicolon, "Expect ';' after return value");
        Ok(Statement::Return { keyword, value })
    }
    fn for_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
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
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
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
            condition,
            body: Box::new(body),
        };
        match initializer {
            None => Ok(body),
            Some(x) => Ok(Statement::Block(vec![x, body])),
        }
    }
    fn while_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        consume!(self, TokenType::LeftParen, "Expect '(' after 'while'.");
        let condition = self.expression()?;
        consume!(
            self,
            TokenType::RightParen,
            "Expect ')' after while condition."
        );
        let body = self.statement()?;
        Ok(Statement::While {
            condition: Some(condition),
            body: Box::new(body),
        })
    }
    fn if_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
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
    fn block(&mut self) -> Result<Vec<Statement<'a>>, ParseError<'a>> {
        let mut statements: Vec<Statement<'a>> = Vec::new();
        while !self.is_at_end() {
            if let TokenType::RightBrace = self.peek()?.tokentype {
                break;
            }
            statements.push(self.declaration()?);
        }
        consume!(self, TokenType::RightBrace, "Expect '}' after block");
        Ok(statements)
    }
    fn print_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let expr = self.expression()?;
        consume!(self, TokenType::Semicolon, "Expect ';' after value.");
        Ok(Statement::Print(expr))
    }
    fn expression_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let expr = self.expression()?;
        consume!(self, TokenType::Semicolon, "Expect ';' after expression.");
        Ok(Statement::Expression(expr))
    }
    fn expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let expr = self.or()?;
        match self.peek()?.tokentype {
            TokenType::Equal => {
                let err = Err(self.error("Invalid assignment target."));
                self.advance();
                let value = self.assignment()?;
                match expr {
                    Expression::Variable { name, scope: _ } => Ok(Expression::Assign {
                        name: name,
                        value: Box::new(value),
                        scope: None,
                    }),
                    Expression::Get { object, name } => Ok(Expression::Set {
                        object,
                        name,
                        value: Box::new(value),
                    }),
                    _ => err,
                }
            }
            _ => Ok(expr),
        }
    }
    fn or(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.and()?;
        while matches!(self, TokenType::Or) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expression::Logical {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn and(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.equality()?;
        while matches!(self, TokenType::And) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expression::Logical {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.comparison()?;
        while matches!(self, TokenType::BangEqual | TokenType::EqualEqual) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.addition()?;
        while matches!(
            self,
            TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual
        ) {
            let operator = self.previous();
            let right = self.addition()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn addition(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.multiplication()?;
        while matches!(self, TokenType::Minus | TokenType::Plus) {
            let operator = self.previous();
            let right = self.multiplication()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn multiplication(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.unary()?;
        while matches!(self, TokenType::Slash | TokenType::Star) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        match self.peek()?.tokentype {
            TokenType::Bang | TokenType::Minus => {
                let operator = self.advance();
                let right = self.unary()?;
                Ok(Expression::Unary {
                    operator: operator,
                    right: Box::new(right),
                })
            }
            _ => self.call(),
        }
    }
    fn call(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let mut expr = self.primary()?;
        loop {
            match self.peek()?.tokentype {
                TokenType::LeftParen => {
                    self.advance();
                    expr = self.finish_call(expr)?;
                }
                TokenType::Dot => {
                    self.advance();
                    let name = consume!(
                        self,
                        TokenType::Identifier,
                        "Expect property name after '.'."
                    );
                    expr = Expression::Get {
                        object: Box::new(expr),
                        name: name,
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn finish_call(&mut self, callee: Expression<'a>) -> Result<Expression<'a>, ParseError<'a>> {
        let mut arguments: Vec<Expression<'a>> = Vec::new();
        match self.peek()?.tokentype {
            TokenType::RightParen => (),
            _ => loop {
                if arguments.len() >= 255 {
                    return Err(self.error("Cannot have more than 255 arguments."));
                }
                arguments.push(self.expression()?);
                match self.peek()?.tokentype {
                    TokenType::Comma => self.advance(),
                    _ => break,
                };
            },
        }
        let paren = consume!(self, TokenType::RightParen, "Expect ')' after arguments.");
        Ok(Expression::Call {
            callee: Box::new(callee),
            paren: paren,
            arguments: arguments,
        })
    }
    fn primary(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        match self.peek()?.tokentype {
            TokenType::False
            | TokenType::True
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => Ok(Expression::Literal(self.advance())),
            TokenType::Identifier(_) => Ok(Expression::Variable {
                name: self.advance(),
                scope: None,
            }),
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                consume!(self, TokenType::RightParen, "Expect ')' after expression.");
                Ok(Expression::Grouping(Box::new(expr)))
            }
            TokenType::This => Ok(Expression::This {
                token: self.advance(),
                scope: None,
            }),
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
    fn advance(&mut self) -> &'a Token {
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
    fn peek(&self) -> Result<&'a Token, ParseError<'a>> {
        match self.tokens.get(self.current) {
            None => Err(self.error("No more tokens")),
            Some(x) => Ok(x),
        }
    }
    fn previous(&self) -> &'a Token {
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
            cur: self.peek().unwrap(),
        }
    }
}

#[cfg(test)]
mod parse_tests {
    use crate::parser;
    use crate::scanner;

    fn expect_success(source: &str) {
        let (tokens, success) = scanner::scan_tokens(source);
        assert!(success);
        let (_, last_err) = parser::parse(&tokens);
        assert!(last_err.is_none(), "{}", last_err.unwrap());
    }

    #[test]
    fn function_declaration() {
        expect_success("fun add(a,b,c) { print a + b + c; }")
    }
}

#[cfg(test)]
mod parse_error_tests {
    use crate::parser;
    use crate::scanner;
    use std::error::Error;

    fn expect_error(source: &str, expected_description: &str) {
        let (tokens, success) = scanner::scan_tokens(source);
        assert!(success);
        let (_, last_err) = parser::parse(&tokens);
        assert!(last_err.is_some());
        assert_eq!(last_err.unwrap().description(), expected_description)
    }

    #[test]
    fn variable_name() {
        expect_error("var 1;", "Expect variable name.")
    }

    #[test]
    fn variable_declaration_no_semicolon() {
        expect_error("var a = 1", "Expect ';' after variable declaration.")
    }

    #[test]
    fn function_no_name() {
        expect_error("fun", "Expect function name")
    }

    #[test]
    fn function_no_left_paren() {
        expect_error("fun foo", "Expect '(' after function name")
    }

    #[test]
    fn function_bad_param() {
        expect_error("fun foo(1)", "Expect parameter name.")
    }

    #[test]
    fn function_no_right_paren() {
        expect_error("fun foo(a", "Expect ')' after parameters.")
    }

    #[test]
    fn function_no_body() {
        expect_error("fun foo(a);", "Expect '{' before function body.")
    }

    #[test]
    fn return_no_semicolon() {
        expect_error("fun foo(a) { return 1 }", "Expect ';' after return value")
    }

    #[test]
    fn for_no_left_paren() {
        expect_error("for", "Expect '(' after 'for'.")
    }

    #[test]
    fn for_no_semicolon() {
        expect_error("for(var a=1; a<10", "Expect ';' after for loop condition.")
    }

    #[test]
    fn for_no_right_paren() {
        expect_error(
            "for(var a=1; a<10; a=a+1",
            "Expect ')' after for loop clauses.",
        )
    }

    #[test]
    fn while_no_left_paren() {
        expect_error("while", "Expect '(' after 'while'.")
    }

    #[test]
    fn while_no_right_paren() {
        expect_error("while(true", "Expect ')' after while condition.")
    }

    #[test]
    fn if_no_left_paren() {
        expect_error("if", "Expect '(' after 'if'.")
    }

    #[test]
    fn if_no_right_paren() {
        expect_error("if(true", "Expect ')' after if condition.")
    }

    #[test]
    fn block_no_right_brace() {
        expect_error("{", "Expect '}' after block")
    }

    #[test]
    fn print_no_semicolon() {
        expect_error("print 1", "Expect ';' after value.")
    }

    #[test]
    fn expression_no_semicolon() {
        expect_error("1", "Expect ';' after expression.")
    }

    #[test]
    fn assignment_invalid_target() {
        expect_error("1 = 2;", "Invalid assignment target.")
    }

    #[test]
    fn fn_call_no_right_paren() {
        expect_error("foo(1", "Expect ')' after arguments.")
    }

    #[test]
    fn grouping_no_right_paren() {
        expect_error("(1", "Expect ')' after expression.")
    }

    #[test]
    fn bad_expression() {
        expect_error("1 + if", "Expect expression.")
    }
}
