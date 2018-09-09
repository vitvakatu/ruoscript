extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod prec_climber;
mod stack;
mod types;
mod value;
mod vm;

use pest::{
    iterators::{Pair, Pairs},
    Parser as PestParser,
};

use prec_climber::{tokenize, Parser};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::str::FromStr;

use ast::BinOp;
use ast::Expr;
use value::Value;
use vm::StorageVar;

const _GRAMMAR: &str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct RuoParser;

fn to_ast(pair: Pair<Rule>) -> Box<Expr> {
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
        Rule::var_decl => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let arg = inner.next().unwrap();
            Box::new(Expr::Assign(ident, to_ast(arg)))
        }
        Rule::var_assign => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let arg = inner.next().unwrap();
            Box::new(Expr::Assign(ident, to_ast(arg)))
        }
        Rule::while_loop => {
            let mut inner = pair.into_inner();
            let cond = to_ast(inner.next().unwrap());
            let code = to_ast(inner.next().unwrap());
            Box::new(Expr::WhileLoop(cond, code))
        }
        Rule::fun_call => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let args = inner.map(to_ast).collect();
            Box::new(Expr::FunCall(ident, args))
        }
        Rule::fun_decl => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let mut body = inner.next().unwrap();
            let mut args = Vec::new();
            while let Some(pair) = inner.next() {
                args.push(body.clone());
                body = pair.clone();
            }
            let args = args.into_iter().map(|p| p.as_str().to_string()).collect();
            Box::new(Expr::FunDecl(ident, args, to_ast(body)))
        }
        Rule::if_cond => {
            let mut inner = pair.into_inner();
            let cond = to_ast(inner.next().unwrap());
            let pos = to_ast(inner.next().unwrap());
            let neg = inner
                .next()
                .map(|p| to_ast(p))
                .unwrap_or(Box::new(Expr::Empty));
            Box::new(Expr::If(cond, pos, neg))
        }
        Rule::return_stmt => {
            Box::new(Expr::Return(to_ast(pair.into_inner().next().unwrap())))
        }
        _ => unreachable!(),
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    let filename = env::args()
        .nth(1)
        .unwrap_or("scripts/arithmetic.ruo".to_string());
    println!("Parsing file: {}", filename);
    let mut file = File::open(filename)?.read_to_string(&mut input)?;

    let ast = RuoParser::parse(Rule::program, &input)
        .unwrap()
        .next()
        .unwrap();

    let ast = to_ast(ast.into_inner().next().unwrap());
    println!("Ast: {:#?}", ast);

    let mut vm = vm::VM::new();
    vm.parse_ast(ast);
    vm.execute();

    Ok(())
}
