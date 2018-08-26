extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod types;
mod value;
mod vm;

use std::fs::File;
use std::io::{self, Read};
use pest::Parser;

use value::Value;

const _GRAMMAR: &str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct RuoParser;

fn print(value: Value) -> Value {
    println!("{:?}", value);
    Value::Empty
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    let mut file = File::open("scripts/arithmetic.ruo")?.read_to_string(&mut input)?;

    let ast = RuoParser::parse(Rule::program, &input).unwrap();
    println!("Ast: {:#?}", ast);
    /*println!("Ast: {:?}", ast);

    let mut vm = vm::VM::new();
    vm.add_variable("print", Value::Function(print));
    vm.parse_ast(ast);
    vm.execute();

    println!("Variables: {:?}", vm.variables());*/

    Ok(())
}
