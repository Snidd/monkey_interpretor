use crate::token::Token;

#[derive(Debug)]
struct Lexer {
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

    pub fn next_token(&mut self) -> Token {
        let result = match self.ch {
            Some('=') => Token::ASSIGN,
            Some(';') => Token::SEMICOLON,
            Some('(') => Token::LPAREN,
            Some(')') => Token::RPAREN,
            Some('{') => Token::LBRACE,
            Some('}') => Token::RBRACE,
            Some(',') => Token::COMMA,
            Some('+') => Token::PLUS,
            None => Token::EOF,
            _ => Token::ILLEGAL,
        };
        self.read_char();
        return result;
    }
}

#[cfg(test)]
#[test]
fn test_next_token() {
    use crate::token;

    let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
";

    let tests = vec![
        (Token::LET, "let"),
        (Token::IDENT("five".to_string()), "five"),
        (Token::ASSIGN, "="),
        (Token::INT(5), "5"),
        (Token::SEMICOLON, ";"),
        (Token::LET, "let"),
        (Token::IDENT("ten".to_string()), "ten"),
        (Token::ASSIGN, "="),
        (Token::INT(10), "10"),
        (Token::SEMICOLON, ";"),
        (Token::LET, "let"),
        (Token::IDENT("add".to_string()), "add"),
        (Token::ASSIGN, "="),
        (Token::FUNCTION, "fn"),
        (Token::LPAREN, "("),
        (Token::IDENT("x".to_string()), "x"),
        (Token::COMMA, ","),
        (Token::IDENT("y".to_string()), "y"),
        (Token::RPAREN, ")"),
        (Token::LBRACE, "{"),
        (Token::IDENT("x".to_string()), "x"),
        (Token::PLUS, "+"),
        (Token::IDENT("y".to_string()), "y"),
        (Token::SEMICOLON, ";"),
        (Token::RBRACE, "}"),
        (Token::SEMICOLON, ";"),
        (Token::LET, "let"),
        (Token::IDENT("result".to_string()), "result"),
        (Token::ASSIGN, "="),
        (Token::IDENT("add".to_string()), "add"),
        (Token::LPAREN, "("),
        (Token::IDENT("five".to_string()), "five"),
        (Token::COMMA, ","),
        (Token::IDENT("ten".to_string()), "ten"),
        (Token::RPAREN, ")"),
        (Token::SEMICOLON, ";"),
        (Token::EOF, ""),
    ];

    let mut lexer = Lexer::new(input);

    for (index, (expected_type, expected_literal)) in tests.into_iter().enumerate() {
        let token = lexer.next_token();
        assert_eq!(token, expected_type, "Index {}", index);
        assert_eq!(token.get_literal(), expected_literal, "Index {}", index);
    }
}
