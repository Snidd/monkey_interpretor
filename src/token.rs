#[derive(PartialEq, Debug)]
pub struct TokenType(pub String);

#[derive(PartialEq, Debug)]
pub struct Token {
    pub literal: String,
    pub token_type: TokenType,
}

impl Token {
    pub fn new(literal: char, token_type: &str) -> Self {
        Self {
            literal: literal.to_string(),
            token_type: TokenType(token_type.to_string()),
        }
    }
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

// Identifiers + literals;
pub const IDENT: &str = "IDENT"; // add, foobar, x, y, ...;
pub const INT: &str = "INT"; // 1343456;
                             // Operators;
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
// Delimiters;
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
// Keywords;
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
