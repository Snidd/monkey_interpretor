#![allow(dead_code)]

use crate::repl::Repl;

mod lexer;
mod repl;
mod token;

fn main() {
    println!("Hello, world!");
    Repl::start();
}
