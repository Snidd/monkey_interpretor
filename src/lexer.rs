use crate::token::{self, Token, TokenType};

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
        if let Some(ch) = self.ch {
            let result = match ch {
                '=' => Token::new(ch, token::ASSIGN),
                ';' => Token::new(ch, token::SEMICOLON),
                '(' => Token::new(ch, token::LPAREN),
                ')' => Token::new(ch, token::RPAREN),
                '{' => Token::new(ch, token::LBRACE),
                '}' => Token::new(ch, token::RBRACE),
                ',' => Token::new(ch, token::COMMA),
                '+' => Token::new(ch, token::PLUS),
                _ => Token::new(ch, token::ILLEGAL),
            };
            self.read_char();
            return result;
        }
        Token {
            literal: "".to_string(),
            token_type: TokenType(token::EOF.to_string()),
        }
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
        (token::LET, "let"),
        (token::IDENT, "five"),
        (token::ASSIGN, "="),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "ten"),
        (token::ASSIGN, "="),
        (token::INT, "10"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "add"),
        (token::ASSIGN, "="),
        (token::FUNCTION, "fn"),
        (token::LPAREN, "("),
        (token::IDENT, "x"),
        (token::COMMA, ","),
        (token::IDENT, "y"),
        (token::RPAREN, ")"),
        (token::LBRACE, "{"),
        (token::IDENT, "x"),
        (token::PLUS, "+"),
        (token::IDENT, "y"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "result"),
        (token::ASSIGN, "="),
        (token::IDENT, "add"),
        (token::LPAREN, "("),
        (token::IDENT, "five"),
        (token::COMMA, ","),
        (token::IDENT, "ten"),
        (token::RPAREN, ")"),
        (token::SEMICOLON, ";"),
        (token::EOF, ""),
    ];

    let mut lexer = Lexer::new(input);

    for (index, (expected_type, expected_literal)) in tests.into_iter().enumerate() {
        let token = lexer.next_token();
        assert_eq!(token.literal, expected_literal, "Index {}", index);
        assert_eq!(
            token.token_type,
            TokenType(expected_type.to_string()),
            "Index {}",
            index
        );
    }
}
