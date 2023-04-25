use std::fmt::Display;

use crate::token::*;
use itertools::Itertools;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseErrors {
    #[error("Invalid Token (expected `{expected:?}` found {found:?})")]
    InvalidToken { expected: String, found: String },
    #[error("Unknown Expression ({expression:?})")]
    UnknownExpression { expression: String },
    #[error("No prefix parse function for {token:?} found")]
    NoPrefixParser { token: Token },
    #[error("Unknown Error")]
    Unknown,
}

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub struct Statement {
    pub statement_type: StatementTypes,
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match &self.statement_type {
            StatementTypes::LetStatement(token, _, _) => format!("{:?}", token),
            StatementTypes::ReturnStatement(token, _) => format!("{:?}", token),
            StatementTypes::ExpressionStatement(token, _) => format!("{:?}", token),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.statement_type {
            StatementTypes::LetStatement(token, name, expr) => {
                return write!(
                    f,
                    "{:?} {} = {};",
                    token,
                    name,
                    format_optional_expression(expr)
                );
            }
            StatementTypes::ReturnStatement(token, expr) => {
                write!(f, "{:?} = {};", token, format_optional_expression(expr))
            }
            StatementTypes::ExpressionStatement(token, expr) => {
                write!(f, "{:?} = {};", token, format_optional_expression(expr))
            }
        }
    }
}

fn format_optional_expression(expr: &Option<Expression>) -> String {
    if expr.is_none() {
        return "None".to_string();
    }

    return format!("{}", expr.as_ref().unwrap());
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression_type {
            ExpressionTypes::Identifier(_token, value) => write!(f, "{}", value.to_string()),
            ExpressionTypes::IntegerLiteral(_, value) => write!(f, "{}", value.to_string()),
            ExpressionTypes::PrefixExpression {
                token: _,
                operator,
                right,
            } => write!(f, "({}{})", operator, right),
        }
    }
}

#[derive(Debug)]
pub enum StatementTypes {
    LetStatement(Token, String, Option<Expression>),
    ReturnStatement(Token, Option<Expression>),
    ExpressionStatement(Token, Option<Expression>),
}

#[derive(Debug)]
pub struct Expression {
    pub expression_type: ExpressionTypes,
}

#[derive(Debug)]
pub enum ExpressionTypes {
    Identifier(Token, String),
    IntegerLiteral(Token, i32),
    PrefixExpression {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        return format!("{}", self);
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParseErrors>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        }
        return "".to_string();
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.statements.iter().format(", "))
    }
}

#[cfg(test)]
#[test]
fn test_string() {
    let program = Program {
        errors: Vec::new(),
        statements: vec![Statement {
            statement_type: StatementTypes::LetStatement(
                Token::LET,
                "myVar".to_string(),
                Some(Expression {
                    expression_type: ExpressionTypes::Identifier(
                        Token::IDENT("anotherVar".to_string()),
                        "anotherVar".to_string(),
                    ),
                }),
            ),
        }],
    };

    let program_str = format!("{}", program);

    assert_eq!(program_str, "LET myVar = anotherVar;");
}
