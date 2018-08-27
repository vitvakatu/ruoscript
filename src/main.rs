extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod prec_climber;
mod types;
mod value;
mod vm;

use pest::{
    iterators::{Pair, Pairs},
    Parser as PestParser
};

use prec_climber::{Parser, tokenize};
use std::fs::File;
use std::io::{self, Read};
use std::str::FromStr;

use ast::BinOp;
use ast::Expr;
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

    let ast = RuoParser::parse(Rule::program, &input)
        .unwrap()
        .next()
        .unwrap();
    println!("Ast: {:#?}", ast);

    let tokens = tokenize(ast.into_inner().next().unwrap());

    let mut parser = Parser::new(tokens.iter());
    let expr = parser.expression(0);

    println!("{:#?}", expr);


    /*println!("Ast: {:?}", ast);

    let mut vm = vm::VM::new();
    vm.add_variable("print", Value::Function(print));
    vm.parse_ast(ast);
    vm.execute();

    println!("Variables: {:?}", vm.variables());*/

    Ok(())
}
