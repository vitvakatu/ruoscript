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

fn to_ast(pair: Pair<Rule>) -> Box<Expr> {
    println!("{:?}", pair);
    match pair.as_rule() {
        Rule::code_block => {
            let mut exprs = Vec::new();
            for each in pair.into_inner() {
                exprs.push(to_ast(each));
            }
            Box::new(Expr::Block(exprs))
        }
        Rule::bin_expr => {
            let tokens = tokenize(pair);
            let mut parser = Parser::new(tokens.iter());
            parser.expression(0)
        }
        Rule::var_assign => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let arg = inner.next().unwrap();
            Box::new(Expr::Assign(ident, to_ast(arg)))
        }
        Rule::fun_call => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let arg = inner.next().unwrap();
            Box::new(Expr::FunCall(ident, to_ast(arg)))
        }
        _ => unreachable!()
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    let mut file = File::open("scripts/arithmetic.ruo")?.read_to_string(&mut input)?;

    let ast = RuoParser::parse(Rule::program, &input)
        .unwrap()
        .next()
        .unwrap();

    let ast = to_ast(ast.into_inner().next().unwrap());
    println!("Ast: {:#?}", ast);

    let mut vm = vm::VM::new();
    vm.add_variable("print", Value::Function(print));
    vm.parse_ast(ast);
    vm.execute();

    println!("Variables: {:?}", vm.variables());

    Ok(())
}
