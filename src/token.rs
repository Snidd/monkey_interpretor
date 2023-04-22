#[derive(PartialEq, Debug)]
pub enum Token {
    IDENT(String),
    INT(i32),
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    ILLEGAL,
    EOF,
}

impl Token {
    pub fn get_literal(&self) -> String {
        match self {
            Token::IDENT(ident) => ident.to_string(),
            Token::INT(number) => number.to_string(),
            Token::ASSIGN => ASSIGN.to_string(),
            Token::PLUS => PLUS.to_string(),
            Token::COMMA => COMMA.to_string(),
            Token::SEMICOLON => SEMICOLON.to_string(),
            Token::LPAREN => LPAREN.to_string(),
            Token::RPAREN => RPAREN.to_string(),
            Token::LBRACE => LBRACE.to_string(),
            Token::RBRACE => RBRACE.to_string(),
            Token::FUNCTION => FUNCTION.to_string(),
            Token::LET => LET.to_string(),
            Token::ILLEGAL => ILLEGAL.to_string(),
            Token::EOF => EOF.to_string(),
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

const ILLEGAL: &str = "ILLEGAL";
const EOF: &str = "EOF";

// Identifiers + literals;
const ASSIGN: &str = "=";
const PLUS: &str = "+";
// Delimiters;
const COMMA: &str = ",";
const SEMICOLON: &str = ";";
const LPAREN: &str = "(";
const RPAREN: &str = ")";
const LBRACE: &str = "{";
const RBRACE: &str = "}";
// Keywords;
const FUNCTION: &str = "FUNCTION";
const LET: &str = "LET";
