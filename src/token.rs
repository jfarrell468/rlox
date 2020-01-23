#[rustfmt::skip]
#[derive(Debug, Clone)]
pub enum TokenType<'a> {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier(&'a str), String(&'a str), Number(f64),

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    EOF
}

pub const TRUE_TOKEN: TokenType = TokenType::True;

#[derive(Debug)]
pub struct Token<'a> {
    pub tokentype: TokenType<'a>,
    pub lexeme: &'a str,
    pub line: i32,
}
