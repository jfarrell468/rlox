use crate::token::{Token, TokenType};
use phf::phf_map;
use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug)]
struct ScanError {
    line: i32,
    message: String,
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.message.as_str())
    }
}

impl Error for ScanError {
    fn description(&self) -> &str {
        &self.message
    }
}

// Note: current becomes self.iter.peek()?.0
struct Scanner<'a> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    start: usize,
    line: i32,
}

pub fn scan_tokens(source: &str) -> (Vec<Token>, bool) {
    let mut scanner = Scanner {
        source: source,
        iter: source.char_indices().peekable(),
        start: 0,
        line: 1,
    };
    let mut tokens: Vec<Token> = Vec::new();

    let mut success = true;
    while let Some((idx, _)) = scanner.iter.peek() {
        scanner.start = *idx;
        match scanner.scan_token() {
            Ok(maybe_token) => {
                if let Some(token) = maybe_token {
                    tokens.push(token);
                }
            }
            Err(e) => {
                eprintln!("{}", e);
                success = false;
            }
        }
    }
    tokens.push(Token {
        tokentype: TokenType::EOF,
        lexeme: String::from(""),
        line: scanner.line,
    });
    (tokens, success)
}

impl<'a> Scanner<'a> {
    fn scan_token(&mut self) -> Result<Option<Token>, Box<dyn Error>> {
        match self.advance()?.1 {
            '(' => Ok(Some(self.token(TokenType::LeftParen))),
            ')' => Ok(Some(self.token(TokenType::RightParen))),
            '{' => Ok(Some(self.token(TokenType::LeftBrace))),
            '}' => Ok(Some(self.token(TokenType::RightBrace))),
            ',' => Ok(Some(self.token(TokenType::Comma))),
            '.' => Ok(Some(self.token(TokenType::Dot))),
            '-' => Ok(Some(self.token(TokenType::Minus))),
            '+' => Ok(Some(self.token(TokenType::Plus))),
            ';' => Ok(Some(self.token(TokenType::Semicolon))),
            '*' => Ok(Some(self.token(TokenType::Star))),
            '!' => {
                if self.next_if('=') {
                    Ok(Some(self.token(TokenType::BangEqual)))
                } else {
                    Ok(Some(self.token(TokenType::Bang)))
                }
            }
            '=' => {
                if self.next_if('=') {
                    Ok(Some(self.token(TokenType::EqualEqual)))
                } else {
                    Ok(Some(self.token(TokenType::Equal)))
                }
            }
            '<' => {
                if self.next_if('=') {
                    Ok(Some(self.token(TokenType::LessEqual)))
                } else {
                    Ok(Some(self.token(TokenType::Less)))
                }
            }
            '>' => {
                if self.next_if('=') {
                    Ok(Some(self.token(TokenType::GreaterEqual)))
                } else {
                    Ok(Some(self.token(TokenType::Greater)))
                }
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
                    Ok(None)
                } else {
                    Ok(Some(self.token(TokenType::Slash)))
                }
            }
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            }
            '"' => Ok(Some(self.string()?)),
            '0'..='9' => Ok(Some(self.number()?)),
            'a'..='z' | 'A'..='Z' | '_' => Ok(Some(self.identifier()?)),
            _ => Err(Box::new(ScanError {
                line: self.line,
                message: "Unexpected character.".to_string(),
            })),
        }
    }
    fn current(&mut self) -> usize {
        match self.iter.peek() {
            None => self.source.len(),
            Some((idx, _)) => *idx,
        }
    }
    fn token(&mut self, token_type: TokenType) -> Token {
        let current = self.current();
        Token {
            tokentype: token_type,
            lexeme: self.source[self.start..current].to_string(),
            line: self.line,
        }
    }
    fn next_if(&mut self, expected: char) -> bool {
        if let Some((_, c)) = self.iter.peek() {
            if *c == expected {
                self.advance().unwrap();
                return true;
            }
        }
        false
    }
    fn advance(&mut self) -> Result<(usize, char), Box<dyn Error>> {
        self.iter.next().ok_or(Box::new(ScanError {
            line: self.line,
            message: "Failed to advance".to_string(),
        }))
    }
    fn string(&mut self) -> Result<Token, Box<dyn Error>> {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                '"' => {
                    break;
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
        if !self.advance().is_ok() {
            return Err(Box::new(ScanError {
                line: self.line,
                message: "Unterminated string.".to_string(),
            }));
        }
        let current = self.current();
        Ok(self.token(TokenType::String(
            self.source[self.start + 1..current - 1].to_string(),
        )))
    }
    fn number(&mut self) -> Result<Token, Box<dyn Error>> {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                '0'..='9' => {
                    self.advance()?;
                }
                _ => {
                    break;
                }
            }
        }

        if let Some((_, c)) = self.iter.peek() {
            if *c == '.' {
                let mut x = self.iter.clone();
                x.next().ok_or(Box::new(ScanError {
                    line: self.line,
                    message: "Failed to advance".to_string(),
                }))?;
                if let Some((_, cc)) = x.peek() {
                    if let '0'..='9' = cc {
                        self.advance()?;
                        while let Some((_, c)) = self.iter.peek() {
                            match c {
                                '0'..='9' => {
                                    self.advance()?;
                                }
                                _ => {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        let current = self.current();
        Ok(self.token(TokenType::Number(self.source[self.start..current].parse()?)))
    }
    fn identifier(&mut self) -> Result<Token, Box<dyn Error>> {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    self.advance()?;
                }
                _ => {
                    break;
                }
            }
        }
        let current = self.current();
        match KEYWORDS.get(&self.source[self.start..current]) {
            None => Ok(self.token(TokenType::Identifier(
                self.source[self.start..current].to_string(),
            ))),
            Some(x) => Ok(self.token(x.clone())),
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

#[cfg(test)]
mod scanner_tests {
    use crate::scanner;
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
        let (tokens, success) = scanner::scan_tokens("x = 2");
        assert!(success);
        assert_eq!(tokens.len(), 4);
        assert!(matches!(tokens[0].tokentype, TokenType::Identifier(_)));
        if let crate::token::TokenType::Identifier(x) = &tokens[0].tokentype {
            assert_eq!(x, "x")
        }
        assert!(matches!(tokens[1].tokentype, TokenType::Equal));
        assert!(matches!(tokens[2].tokentype, TokenType::Number(_)));
        if let crate::token::TokenType::Number(x) = tokens[2].tokentype {
            assert_eq!(x, 2.0)
        }
        assert!(matches!(tokens[3].tokentype, TokenType::EOF));
    }

    #[test]
    fn number_parsing() {
        let (tokens, success) = scanner::scan_tokens("1+2");
        assert!(success);
        assert_eq!(tokens.len(), 4);
        assert!(matches!(tokens[0].tokentype, TokenType::Number(_)));
        if let crate::token::TokenType::Number(x) = tokens[0].tokentype {
            assert_eq!(x, 1.0)
        }
        assert!(matches!(tokens[1].tokentype, TokenType::Plus));
        assert!(matches!(tokens[2].tokentype, TokenType::Number(_)));
        if let crate::token::TokenType::Number(x) = tokens[2].tokentype {
            assert_eq!(x, 2.0)
        }
        assert!(matches!(tokens[3].tokentype, TokenType::EOF));
    }
}
