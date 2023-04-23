use crate::token::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseErrors {
    #[error("Invalid Token (expected `{expected:?}` found {found:?})")]
    InvalidToken { expected: String, found: String },
    #[error("Unknown Expression ({expression:?})")]
    UnknownExpression { expression: String },
    #[error("Unknown Error")]
    Unknown,
}

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
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

    fn string(&self) -> String {
        match &self.statement_type {
            StatementTypes::LetStatement(token, name, expr) => {
                return format!(
                    "{:?} {} = {};",
                    token,
                    name,
                    expr.as_ref().unwrap().string()
                );
            }
            StatementTypes::ReturnStatement(token, expr) => {
                format!("{:?} = {};", token, expr.as_ref().unwrap().string())
            }
            StatementTypes::ExpressionStatement(token, expr) => {
                format!("{:?} = {};", token, expr.as_ref().unwrap().string())
            }
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
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match &self.expression_type {
            ExpressionTypes::Identifier(token, _) => format!("{:?}", token),
            ExpressionTypes::IntegerLiteral(token, _) => format!("{:?}", token),
        }
    }

    fn string(&self) -> String {
        match &self.expression_type {
            ExpressionTypes::Identifier(_token, value) => value.to_string(),
            ExpressionTypes::IntegerLiteral(_, value) => value.to_string(),
        }
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
    pub fn string(&self) -> String {
        return self.statements.iter().map(|s| s.string()).collect();
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

    assert_eq!(program.string(), "LET myVar = anotherVar;");
}
