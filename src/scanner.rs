use super::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::CharIndices;

use phf::phf_map;

// Note: current becomes self.iter.peek()?.0
pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    iter: Peekable<CharIndices<'a>>,
    start: usize,
    line: i32,
}

impl<'a> Scanner<'a> {
    pub fn from_string(source: &str) -> Scanner {
        Scanner { source: source, tokens: Vec::new(), iter: source.char_indices().peekable(), start: 0, line: 1 }
    }
    pub fn scan_tokens(&mut self) -> (&Vec<Token>, bool) {
        let mut err = false;
        while let Some((idx, _)) = self.iter.peek() {
            self.start = *idx;
            if let Err(_) = self.scan_token() {
                err = true;
            }
        }
        self.tokens.push(Token { tokentype: TokenType::EOF, lexeme: "", line: self.line });
        (&self.tokens, !err)
    }
    fn scan_token(&mut self) -> Result<(), ()> {
        if let Some((_, c)) = self.iter.next() {
            match c {
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
                '!' => {
                    let token = if self.next_if('=') { TokenType::BangEqual } else { TokenType::Bang };
                    self.add_token(token)
                }
                '=' => {
                    let token = if self.next_if('=') { TokenType::EqualEqual } else { TokenType::Equal };
                    self.add_token(token)
                }
                '<' => {
                    let token = if self.next_if('=') { TokenType::LessEqual } else { TokenType::Less };
                    self.add_token(token)
                }
                '>' => {
                    let token = if self.next_if('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                    self.add_token(token)
                }
                '/' => {
                    if self.next_if('/') {
                        while let Some((_, c)) = self.iter.peek() {
                            match c {
                                '\n' => {
                                    break;
                                }
                                _ => {
                                    self.iter.next();
                                }
                            }
                        }
                    } else {
                        self.add_token(TokenType::Slash);
                    }
                }
                ' ' | '\r' | '\t' => (),
                '\n' => { self.line += 1; }
                '"' => self.string(),
                '0'..='9' => self.number(),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
                _ => {
                    error(self.line, "Unexpected character");
                    return Err(());
                }
            }
        }
        Ok(())
    }
    fn current(&mut self) -> usize {
        match self.iter.peek() {
            None => self.source.len(),
            Some((idx, _)) => *idx
        }
    }
    fn add_token(&mut self, token_type: TokenType<'a>) {
        let current = self.current();
        self.tokens.push(Token { tokentype: token_type, lexeme: &self.source[self.start..current], line: self.line })
    }
    fn next_if(&mut self, expected: char) -> bool {
        if let Some((_, c)) = self.iter.peek() {
            if *c == expected {
                self.iter.next().expect("Failed to advance.");
                return true;
            }
        }
        false
    }
    fn string(&mut self) {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                '"' => { break; }
                '\n' => {
                    self.line += 1;
                    self.iter.next();
                }
                _ => { self.iter.next(); }
            }
        }

        if self.iter.peek().is_none() {
            error(self.line, "Unterminated string.");
            return;
        }

        self.iter.next().expect("Failed to advance.");

        let current = self.current();
        self.add_token(TokenType::String(&self.source[self.start + 1..current - 1]));
    }
    fn number(&mut self) {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                '0'..='9' => { self.iter.next(); }
                _ => { break; }
            }
        }

        if let Some((_, _)) = self.iter.peek() {
            let mut x = self.iter.clone();
            x.next().expect("failed to advance");
            if let Some((_, cc)) = x.peek() {
                match cc {
                    '0'..='9' => {
                        self.iter.next();
                        while let Some((_, c)) = self.iter.peek() {
                            match c {
                                '0'..='9' => { self.iter.next(); }
                                _ => { break; }
                            }
                        }
                    }
                    _ => ()
                }
            }
        }

        let current = self.current();
        self.add_token(TokenType::Number(self.source[self.start..current].parse().expect("failed to parse number")));
    }
    fn identifier(&mut self) {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => { self.iter.next(); }
                _ => { break; }
            }
        }
        let current = self.current();
        match KEYWORDS.get(&self.source[self.start..current]) {
            None => { self.add_token(TokenType::Identifier(&self.source[self.start..current])); }
            Some(x) => { self.add_token(x.clone()); }
        }
    }
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "for" => TokenType::For,
    "fun" => TokenType::Fun,
    "if" => TokenType::If,
    "nil" => TokenType::Nil,
    "or" => TokenType::Or,
    "print" => TokenType::Print,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
};

fn error(line: i32, message: &str) {
    report(line, "", message);
}

fn report(line: i32, wher: &str, message: &str) {
    println!("[line {}] Error{}: {}", line, wher, message);
}

#[cfg(test)]
mod scanner_tests {
    use crate::scanner::Scanner;
    use crate::token::TokenType;

    // TODO: Just use the "matches" crate.
    macro_rules! matches {
        ($expression:expr, $($pattern:tt)+) => {
            match $expression {
                $($pattern)+ => true,
                _ => false
            }
        }
    }

    #[test]
    fn basic_scanner_test() {
        let mut scanner = Scanner::from_string("x = 2");
        let (tokens, success) = scanner.scan_tokens();
        assert!(success);
        assert_eq!(tokens.len(), 4);
        assert!(matches!(tokens[0].tokentype, TokenType::Identifier("x")));
        assert!(matches!(tokens[1].tokentype, TokenType::Equal));
        assert!(matches!(tokens[2].tokentype, TokenType::Number(_)));
        if let crate::token::TokenType::Number(x) = tokens[2].tokentype {
            assert_eq!(x, 2.0)
        }
        assert!(matches!(tokens[3].tokentype, TokenType::EOF));
    }
}