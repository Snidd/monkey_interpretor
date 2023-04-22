use crate::token::Token;
#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.to_string(),
            ch: input.chars().nth(0),
            position: 0,
            read_position: 1,
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position.clone();
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        return self.input.chars().nth(self.read_position);
    }

    fn read_identifier(&mut self) -> String {
        let start_position = self.position.clone();
        while self.ch.is_some_and(|char| char.is_alphabetic()) {
            self.read_char();
        }
        return self.input[start_position..self.position].to_string();
    }

    fn read_number(&mut self) -> i32 {
        let start_position = self.position.clone();
        while self.ch.is_some_and(|char| char.is_numeric()) {
            self.read_char();
        }
        return self.input[start_position..self.position]
            .parse()
            .unwrap_or(0);
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some_and(|char| char.is_whitespace()) {
            println!(
                "skipping whitespace at {} [{}]",
                self.position,
                self.ch.unwrap()
            );
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        println!("{:?}", self);
        self.skip_whitespace();
        let mut read_next = true;
        let token = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            Some(';') => Token::SEMICOLON,
            Some('-') => Token::MINUS,
            Some('!') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::NOT_EQ
                } else {
                    Token::BANG
                }
            }
            Some('/') => Token::SLASH,
            Some('*') => Token::ASTERISK,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some('(') => Token::LPAREN,
            Some(')') => Token::RPAREN,
            Some('{') => Token::LBRACE,
            Some('}') => Token::RBRACE,
            Some(',') => Token::COMMA,
            Some('+') => Token::PLUS,
            None => Token::EOF,
            option_char => {
                let char = option_char.unwrap();
                if char.is_ascii_alphabetic() {
                    let literal = self.read_identifier();
                    read_next = false;
                    Token::lookup_token(&literal)
                } else if char.is_numeric() {
                    let literal = self.read_number();
                    read_next = false;
                    Token::INT(literal)
                } else {
                    Token::ILLEGAL(char.to_string())
                }
            }
        };
        println!("{:?} {}", token, self.position);
        if read_next {
            self.read_char();
        }
        return token;
    }
}

#[cfg(test)]
#[test]
fn test_six_basic_lines() {
    let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
";

    let tests = vec![
        Token::LET,
        Token::IDENT("five".to_string()),
        Token::ASSIGN,
        Token::INT(5),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("ten".to_string()),
        Token::ASSIGN,
        Token::INT(10),
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
        Token::EOF,
    ];

    run_tests(input, tests);
}

#[test]
fn test_double_operators() {
    let input = "10 == 10;
11 != 9;";

    let tests = vec![
        Token::INT(10),
        Token::EQ,
        Token::INT(10),
        Token::SEMICOLON,
        Token::INT(11),
        Token::NOT_EQ,
        Token::INT(9),
        Token::SEMICOLON,
    ];

    run_tests(input, tests);
}

#[test]
fn test_six_extended_lines() {
    let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
    } else {
    return false;
}
";

    let tests = vec![
        Token::LET,
        Token::IDENT("five".to_string()),
        Token::ASSIGN,
        Token::INT(5),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("ten".to_string()),
        Token::ASSIGN,
        Token::INT(10),
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
        Token::INT(5),
        Token::SEMICOLON,
        Token::INT(5),
        Token::LT,
        Token::INT(10),
        Token::GT,
        Token::INT(5),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT(5),
        Token::LT,
        Token::INT(10),
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
        Token::EOF,
    ];

    run_tests(input, tests);
}

fn run_tests(input: &str, tests: Vec<Token>) {
    let mut lexer = Lexer::new(input);

    for (index, expected_type) in tests.into_iter().enumerate() {
        let token = lexer.next_token();
        assert_eq!(token, expected_type, "Index {}", index);
    }
}
