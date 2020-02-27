use strum_macros::Display;

#[rustfmt::skip]
#[derive(Debug, Clone, Display, Copy)]
#[repr(u8)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    EOF
}

#[derive(Debug)]
pub struct Token<'a> {
    pub tokentype: TokenType,
    pub lexeme: &'a str,
    pub line: i32,
}
