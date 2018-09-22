extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
//mod prec_climber;
mod stack;
mod types;
mod value;
//mod vm;
mod parser;

use pest::{iterators::Pair, Parser as PestParser};

use prec_climber::{tokenize, Parser};
use std::env;
use std::fs::File;
use std::io::{self, Read};

use ast::{BinOp, Expr};

use parser::to_ast;

fn main() -> io::Result<()> {
    /*let filename = env::args()
        .nth(1)
        .unwrap_or("scripts/arithmetic.ruo".to_string());
    println!("Parsing file: {}", filename);

    println!("Ast: {:#?}", ast);

    let mut vm = vm::VM::new();
    vm.parse_ast(ast);
    vm.execute().unwrap();
*/
    Ok(())
}
