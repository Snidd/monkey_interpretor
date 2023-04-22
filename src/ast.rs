use crate::{lexer::Lexer, token::*};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub struct Statement {
    pub statement_type: StatementTypes,
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match &self.statement_type {
            StatementTypes::LetStatement(token, _, _) => format!("{:?}", token),
        }
    }
}

pub enum StatementTypes {
    LetStatement(Token, String, Option<Expression>),
}
pub struct Expression {
    pub expression_type: ExpressionTypes,
}
pub enum ExpressionTypes {
    Identifier(Token, String),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match &self.expression_type {
            ExpressionTypes::Identifier(token, _) => format!("{:?}", token),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        }
        return "".to_string();
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer: lexer,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token != Token::EOF {
            let statement_opt = self.parse_statement();
            if let Some(statement) = statement_opt {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TYPE_IDENT) {
            return None;
        }

        if let Token::IDENT(ident) = self.cur_token.clone() {
            if !self.expect_peek(TYPE_ASSIGN) {
                return None;
            }

            //Skip to semicolon for now
            while self.cur_token == Token::SEMICOLON {
                self.next_token();
            }

            return Some(Statement {
                statement_type: StatementTypes::LetStatement(token, ident.to_string(), None),
            });
        }

        return None;
    }

    fn cur_token_is(&self, expected_token: &str) -> bool {
        self.cur_token.get_type() == expected_token
    }

    fn peek_token_is(&self, expected_token: &str) -> bool {
        self.peek_token.get_type() == expected_token
    }

    fn expect_peek(&mut self, expected_token: &str) -> bool {
        if self.peek_token_is(expected_token) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }
}

#[cfg(test)]
#[test]
fn test_let_statements() {
    let input = "let x = 5;
let y = 10;
let foobar = 838383;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    assert_eq!(program.is_some(), true, "Program didnt return a program!");
    let program = program.unwrap();
    assert_eq!(program.statements.len(), 3);

    let tests = vec!["x", "y", "foobar"];

    for (index, expected_identifier) in tests.into_iter().enumerate() {
        let statement = &program.statements[index];
        if !test_let_statement(statement, expected_identifier) {
            return;
        }
    }
}

fn test_let_statement(statement: &Statement, expected_name: &str) -> bool {
    assert_eq!(statement.token_literal(), "LET");

    if let StatementTypes::LetStatement(token, name, expr) = &statement.statement_type {
        assert_eq!(name, expected_name);
        return false;
    } else {
        return true;
    }
}
