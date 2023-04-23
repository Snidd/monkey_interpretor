#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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

    pub fn get_type(&self) -> String {
        let token_type = match self {
            Token::IDENT(_) => TYPE_IDENT,
            Token::INT(_) => TYPE_INT,
            Token::ASSIGN => TYPE_ASSIGN,
            Token::PLUS => TYPE_PLUS,
            Token::MINUS => TYPE_MINUS,
            Token::BANG => TYPE_BANG,
            Token::ASTERISK => TYPE_ASTERISK,
            Token::SLASH => TYPE_SLASH,
            Token::LT => TYPE_LT,
            Token::GT => TYPE_GT,
            Token::EQ => TYPE_EQ,
            Token::NOT_EQ => TYPE_NOT_EQ,
            Token::COMMA => TYPE_COMMA,
            Token::SEMICOLON => TYPE_SEMICOLON,
            Token::LPAREN => TYPE_LPAREN,
            Token::RPAREN => TYPE_RPAREN,
            Token::LBRACE => TYPE_LBRACE,
            Token::RBRACE => TYPE_RBRACE,
            Token::FUNCTION => TYPE_FUNCTION,
            Token::LET => TYPE_LET,
            Token::TRUE => TYPE_TRUE,
            Token::FALSE => TYPE_FALSE,
            Token::IF => TYPE_IF,
            Token::ELSE => TYPE_ELSE,
            Token::RETURN => TYPE_RETURN,
            Token::ILLEGAL(_) => TYPE_ILLEGAL,
            Token::EOF => TYPE_EOF,
        };
        return token_type.to_string();
    }
}

pub const TYPE_IDENT: &str = "IDENT";
pub const TYPE_INT: &str = "INT";
pub const TYPE_ASSIGN: &str = "ASSIGN";
pub const TYPE_PLUS: &str = "PLUS";
pub const TYPE_MINUS: &str = "MINUS";
pub const TYPE_BANG: &str = "BANG";
pub const TYPE_ASTERISK: &str = "ASTERISK";
pub const TYPE_SLASH: &str = "SLASH";
pub const TYPE_LT: &str = "LT";
pub const TYPE_GT: &str = "GT";
pub const TYPE_EQ: &str = "EQ";
pub const TYPE_NOT_EQ: &str = "NOT_EQ";
pub const TYPE_COMMA: &str = "COMMA";
pub const TYPE_SEMICOLON: &str = "SEMICOLON";
pub const TYPE_LPAREN: &str = "LPAREN";
pub const TYPE_RPAREN: &str = "RPAREN";
pub const TYPE_LBRACE: &str = "LBRACE";
pub const TYPE_RBRACE: &str = "RBRACE";
pub const TYPE_FUNCTION: &str = "FUNCTION";
pub const TYPE_LET: &str = "LET";
pub const TYPE_TRUE: &str = "TRUE";
pub const TYPE_FALSE: &str = "FALSE";
pub const TYPE_IF: &str = "IF";
pub const TYPE_ELSE: &str = "ELSE";
pub const TYPE_RETURN: &str = "RETURN";
pub const TYPE_ILLEGAL: &str = "ILLEGAL";
pub const TYPE_EOF: &str = "EOF";

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
