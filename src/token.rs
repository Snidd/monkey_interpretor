#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug)]
pub enum Token {
    IDENT(String),
    INT(i32),
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    ILLEGAL(String),
    EOF,
}

impl Token {
    pub fn lookup_token(ident: &str) -> Token {
        match ident {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            identifier => Token::IDENT(identifier.to_string()),
        }
    }
}

/* #[derive(PartialEq, Debug)]
pub struct Token {
    pub literal: String,
    pub token_type: TokenType,
} */

/* impl Token {
    pub fn new(literal: char, token_type: &str) -> Self {
        Self {
            literal: literal.to_string(),
            token_type: TokenType(token_type.to_string()),
        }
    }
} */
