#[macro_use]
extern crate log;
extern crate env_logger;
extern crate failure;
#[macro_use]
extern crate failure_derive;

mod parser;
mod lexer;
mod stack;
mod types;
mod value;

use std::env;
use std::fs::File;
use std::io::{self, Write};
use std::process::Command;

fn main() -> io::Result<()> {
    env_logger::init();
    let src = "def foo(x, y) { x + y };";
    let mut lexer = lexer::Lexer::new(src.char_indices());
    let tokens: Vec<_> = lexer.get_tokens().unwrap().iter().map(|span| span.inner.clone()).collect();
    let mut parser = parser::Parser::new(tokens.iter());

    parser.parse();

    Ok(())
}
