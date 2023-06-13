#[derive(PartialEq, Debug)]
pub enum Token {
    ILLEGAL(String),
    EOF,

    IDENT(String),
    INT(String),
    STRING(String),

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NotEq,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Token {
    pub fn literal(&self) -> &str {
        match self {
            Token::ILLEGAL(v) | Token::IDENT(v) | Token::INT(v) | Token::STRING(v) => v,
            Token::EOF => "",
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NotEq => "!=",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRACE => "{",
            Token::RBRACE => "}",
            Token::LBRACKET => "[",
            Token::RBRACKET => "]",
            Token::FUNCTION => "function",
            Token::LET => "let",
            Token::TRUE => "true",
            Token::FALSE => "false",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::RETURN => "return",
        }
    }
}
