use super::token::{Token, TokenType, Literal};
use std::ops::Index;

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: i32,
}

impl<'a> Scanner<'a> {
    pub fn from_string(source: &str) -> Scanner {
        Scanner { source: source, tokens: Vec::new(), start: 0, current: 0, line: 1 }
    }
    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens.push(Token{ tokentype: TokenType::EOF, lexeme: "", literal: None, line: self.line });
        return &self.tokens;
    }
    fn scan_token(&mut self) {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            _ => (),
        }
    }
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
    fn advance(&mut self) -> char {
        self.current += 1;
        // TODO: This is inefficient.
        self.source.chars().nth(self.current - 1).unwrap()
    }
    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_and_literal(token_type, None)
    }
    fn add_token_and_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        self.tokens.push(Token{ tokentype: token_type, lexeme: &self.source[self.start..self.current], literal: literal, line: self.line })
    }
}