use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut s = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        s.read_char();
        s
    }

    pub fn next_token(self: &mut Self) -> Token {
        self.skip_whitespace();
        let tok = match (self.ch, self.peek_char()) {
            ('=', '=') => {
                self.read_char();
                Token::EQ
            }
            ('=', _) => Token::ASSIGN,
            (';', _) => Token::SEMICOLON,
            ('(', _) => Token::LPAREN,
            (')', _) => Token::RPAREN,
            ('{', _) => Token::LBRACE,
            ('}', _) => Token::RBRACE,
            (',', _) => Token::COMMA,
            ('+', _) => Token::PLUS,
            ('-', _) => Token::MINUS,
            ('!', '=') => {
                self.read_char();
                Token::NotEq
            }
            ('!', _) => Token::BANG,
            ('*', _) => Token::ASTERISK,
            ('/', _) => Token::SLASH,
            ('<', _) => Token::LT,
            ('>', _) => Token::GT,
            ('\0', _) => Token::EOF,
            ('"', _) => {
                let literal = self.read_string();
                Token::STRING(literal)
            }
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    return match ident.as_str() {
                        "let" => Token::LET,
                        "fn" => Token::FUNCTION,
                        "true" => Token::TRUE,
                        "false" => Token::FALSE,
                        "if" => Token::IF,
                        "else" => Token::ELSE,
                        "return" => Token::RETURN,
                        _ => Token::IDENT(ident),
                    };
                } else if is_digit(self.ch) {
                    return Token::INT(self.read_number());
                } else {
                    Token::ILLEGAL(self.ch.to_string())
                }
            }
        };
        self.read_char();

        tok
    }

    fn read_char(self: &mut Self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(self: &Self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn read_identifier(self: &mut Self) -> String {
        let start_position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[start_position..self.position].iter().collect()
    }

    fn read_string(self: &mut Self) -> String {
        self.read_char();
        let start_position = self.position;
        while self.ch != '"' || self.ch == '\0' {
            self.read_char();
        }
        self.input[start_position..self.position].iter().collect()
    }

    fn read_number(self: &mut Self) -> String {
        let start_position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[start_position..self.position].iter().collect()
    }

    fn skip_whitespace(self: &mut Self) {
        while is_whitespace(self.ch) {
            self.read_char();
        }
    }
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

fn is_letter(ch: char) -> bool {
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

#[test]
fn test_next_token() {
    let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;

\"foobar\"
\"foo bar\"
";

    let tests = [
        Token::LET,
        Token::IDENT("five".to_string()),
        Token::ASSIGN,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("ten".to_string()),
        Token::ASSIGN,
        Token::INT("10".to_string()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("add".to_string()),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENT("x".to_string()),
        Token::COMMA,
        Token::IDENT("y".to_string()),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENT("x".to_string()),
        Token::PLUS,
        Token::IDENT("y".to_string()),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("result".to_string()),
        Token::ASSIGN,
        Token::IDENT("add".to_string()),
        Token::LPAREN,
        Token::IDENT("five".to_string()),
        Token::COMMA,
        Token::IDENT("ten".to_string()),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERISK,
        Token::INT("5".to_string()),
        Token::INT("5".to_string()),
        Token::LT,
        Token::INT("10".to_string()),
        Token::GT,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT("5".to_string()),
        Token::LT,
        Token::INT("10".to_string()),
        Token::RPAREN,
        Token::LBRACE,
        Token::RETURN,
        Token::TRUE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::ELSE,
        Token::LBRACE,
        Token::RETURN,
        Token::FALSE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::INT("10".to_string()),
        Token::EQ,
        Token::INT("10".to_string()),
        Token::SEMICOLON,
        Token::INT("10".to_string()),
        Token::NotEq,
        Token::INT("9".to_string()),
        Token::SEMICOLON,
        Token::STRING("foobar".to_string()),
        Token::STRING("foo bar".to_string()),
        Token::EOF,
    ];

    let mut l = Lexer::new(input);

    for (i, t) in tests.iter().enumerate() {
        let token = l.next_token();

        assert_eq!(*t, token, "xxx {}", i);
    }
}
