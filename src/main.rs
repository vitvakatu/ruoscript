#[macro_use]
extern crate log;
extern crate env_logger;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate llvm_sys;

mod codegen;
mod executor;
mod lexer;
mod parser;
mod stack;
mod types;
mod value;

use std::env;
use std::fs::File;
use std::io::{self, Write};
use std::process::Command;

fn main() -> io::Result<()> {
    env_logger::init();
    let src = "1 + 2";
    let mut lexer = lexer::Lexer::new(src.char_indices());
    let tokens: Vec<_> = lexer.get_tokens().unwrap();
    let mut parser = parser::Parser::new(tokens.iter());

    let mut context = codegen::Context::new();
    parser.parse(&mut context).unwrap();

    Ok(())
}
