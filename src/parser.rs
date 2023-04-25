use crate::{
    ast::{
        self, Expression, ExpressionTypes, Node, ParseErrors, Program, Statement, StatementTypes,
    },
    lexer::{self},
    token::*,
};

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

type PrefixParseFn = fn() -> ast::Expression;
type InfixParseFn = fn(ast::Expression) -> ast::Expression;

struct Parser {
    lexer: lexer::Lexer,
    errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(mut lexer: lexer::Lexer) -> Self {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer: lexer,
            errors: Vec::new(),
        }
    }

    fn run_prefix(&mut self, token: &Token) -> Result<Expression, ParseErrors> {
        match token {
            Token::IDENT(ident) => Ok(self.parse_identifier(ident)),
            Token::INT(value) => Ok(self.parse_integer_literal(value.clone())),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),
            _ => Err(ParseErrors::NoPrefixParser {
                token: token.clone(),
            }),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseErrors>> {
        let mut program = Program {
            statements: Vec::new(),
            errors: Vec::new(),
        };

        while self.cur_token != Token::EOF {
            let statement_result = self.parse_statement();
            match statement_result {
                Ok(statement) => {
                    program.statements.push(statement);
                }
                Err(err) => {
                    program.errors.push(err);
                }
            }
            self.next_token();
        }

        if program.errors.len() == 0 {
            Ok(program)
        } else {
            Err(program.errors)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseErrors> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseErrors> {
        let current_token = self.cur_token.clone();

        self.next_token();

        let right_expression = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression {
            expression_type: ExpressionTypes::PrefixExpression {
                token: current_token.clone(),
                operator: format!("{}", current_token.get_type()),
                right: Box::new(right_expression),
            },
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseErrors> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TYPE_SEMICOLON) {
            self.next_token();
        }

        return Ok(Statement {
            statement_type: StatementTypes::ExpressionStatement(token, Some(expression)),
        });
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseErrors> {
        let _precedence = precedence as isize;
        let token = self.cur_token.clone();
        let prefix = self.run_prefix(&token);
        return prefix;
    }

    pub fn parse_identifier(&self, ident: &str) -> Expression {
        return Expression {
            expression_type: ExpressionTypes::Identifier(self.cur_token.clone(), ident.to_string()),
        };
    }

    pub fn parse_integer_literal(&self, value: i32) -> Expression {
        return Expression {
            expression_type: ExpressionTypes::IntegerLiteral(self.cur_token.clone(), value),
        };
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseErrors> {
        let token = self.cur_token.clone();
        //Skip to semicolon for now
        while self.cur_token != Token::SEMICOLON {
            self.next_token();
        }

        return Ok(Statement {
            statement_type: StatementTypes::ReturnStatement(token, None),
        });
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseErrors> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TYPE_IDENT) {
            return Err(ParseErrors::InvalidToken {
                expected: TYPE_IDENT.to_string(),
                found: self.peek_token.get_type(),
            });
        }

        if let Token::IDENT(ident) = self.cur_token.clone() {
            if !self.expect_peek(TYPE_ASSIGN) {
                return Err(ParseErrors::InvalidToken {
                    expected: TYPE_ASSIGN.to_string(),
                    found: self.peek_token.get_type(),
                });
            }

            //Skip to semicolon for now
            while self.cur_token != Token::SEMICOLON {
                self.next_token();
            }

            return Ok(Statement {
                statement_type: StatementTypes::LetStatement(token, ident.to_string(), None),
            });
        }

        return Err(ParseErrors::Unknown);
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
use crate::lexer::Lexer;

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];
    for (input, _operator, _value) in prefix_tests {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let program = check_parser_errors(program);

        if program.statements.len() != 1 {
            assert!(
                false,
                "Should have 1 statement, got {}",
                program.statements.len()
            )
        }

        let statement = &program.statements[0];
        match &statement.statement_type {
            StatementTypes::ExpressionStatement(_, expr) => {
                if let Some(expression) = expr {
                    match &expression.expression_type {
                        ExpressionTypes::PrefixExpression {
                            token: _,
                            operator: _,
                            right: _,
                        } => (),
                        _ => assert!(false, "Expression should be prefix expression"),
                    }
                } else {
                    assert!(false, "Should have an expression!")
                }
            }
            _ => assert!(false, "Statement should be ExpressionStatement"),
        }
    }
}

#[test]
fn test_integer_literal() {}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let lexer = lexer::Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = check_parser_errors(parser.parse_program());

    assert_eq!(program.statements.len(), 1);

    let statement = &program.statements[0];
    match &statement.statement_type {
        StatementTypes::ExpressionStatement(_token, expr) => {
            if let Some(expr) = expr {
                match &expr.expression_type {
                    ast::ExpressionTypes::Identifier(token, ident) => {
                        assert_eq!(ident, "foobar");
                        assert_eq!(token.clone(), Token::IDENT("foobar".to_string()));
                    }
                    _ => assert!(false, "Should be Identifier"),
                }
            }
        }
        _ => assert!(false, "Expected ExpressionStatement"),
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";

    let lexer = lexer::Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = check_parser_errors(parser.parse_program());

    assert_eq!(program.statements.len(), 1);

    let statement = &program.statements[0];
    match &statement.statement_type {
        StatementTypes::ExpressionStatement(_token, expr) => {
            if let Some(expr) = expr {
                match &expr.expression_type {
                    ast::ExpressionTypes::IntegerLiteral(token, value) => {
                        assert_eq!(value.to_owned(), 5 as i32);
                        assert_eq!(token.clone(), Token::INT(5));
                    }
                    _ => assert!(false, "Should be Integer literal"),
                }
            }
        }
        _ => assert!(false, "Expected ExpressionStatement"),
    }
}

#[test]
fn test_return_statements() {
    let input = "return 5;
return 10;
return 993322;";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let program = check_parser_errors(program);

    assert_eq!(program.statements.len(), 3);

    for (_index, statement) in program.statements.iter().enumerate() {
        match &statement.statement_type {
            StatementTypes::ReturnStatement(_, _) => (),
            statement_type => assert!(false, "Should be ReturnStatement, got {:?}", statement_type),
        }
    }
}

#[test]
fn test_let_statements() {
    let input = "let x = 5;
let y = 10;
let foobar = 838383;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    match program {
        Err(errors) => {
            println!("{:?}", errors);
            assert!(false, "Didnt expect errors in the parsing");
        }
        Ok(program) => {
            assert_eq!(program.statements.len(), 3);

            let tests = vec!["x", "y", "foobar"];

            for (index, expected_identifier) in tests.into_iter().enumerate() {
                let statement = &program.statements[index];
                if !test_let_statement(statement, expected_identifier) {
                    return;
                }
            }
        }
    }
}

fn test_let_statement(statement: &Statement, expected_name: &str) -> bool {
    assert_eq!(statement.token_literal(), "LET");

    if let StatementTypes::LetStatement(_token, name, _expr) = &statement.statement_type {
        assert_eq!(name, expected_name);
        return false;
    } else {
        return true;
    }
}

fn check_parser_errors(result: Result<Program, Vec<ParseErrors>>) -> Program {
    if let Err(errors) = result {
        println!("{:?}", errors);
        assert!(false);
        return Program {
            errors: errors,
            statements: Vec::new(),
        };
    } else {
        return result.unwrap();
    }
}
