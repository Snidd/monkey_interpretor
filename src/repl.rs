use std::io::{self, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub struct Repl;

impl Repl {
    pub fn start() {
        print!("{}", PROMPT);
        let res = io::stdout().flush();
        if res.is_err() {
            println!("Unable to print to stdout!");
            return;
        }

        let mut user_input = String::new();
        let stdin = io::stdin();

        let res = stdin.read_line(&mut user_input);
        if res.is_err() {
            println!("Unable to read from stdin!");
            return;
        }

        let mut lexer = Lexer::new(&user_input);

        loop {
            let token = lexer.next_token();
            if token == Token::EOF {
                break;
            }
            println!("{:?}", token);
        }
    }
}
