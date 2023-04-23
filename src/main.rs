#![allow(dead_code)]

use crate::repl::Repl;

mod ast;
mod lexer;
mod repl;
mod token;
mod parser;

fn main() {
    println!("Hello, world!");
    Repl::start();
}
