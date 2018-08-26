extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod types;
mod value;
mod vm;

use std::fs::File;
use std::io::{self, Read};
use pest::{Parser, iterators::{Pair, Pairs}};
use std::str::FromStr;
use pest::prec_climber::{PrecClimber, Operator, Assoc};

use value::Value;
use ast::Expr;
use ast::BinOp;

const _GRAMMAR: &str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct RuoParser;

fn print(value: Value) -> Value {
    println!("{:?}", value);
    Value::Empty
}

impl Rule {
    pub fn to_bin_op(&self) -> Option<BinOp> {
        match *self {
            Rule::op_add => Some(BinOp::Add),
            Rule::op_sub => Some(BinOp::Sub),
            Rule::op_mul => Some(BinOp::Mul),
            Rule::op_div => Some(BinOp::Div),
            Rule::op_pow => Some(BinOp::Pow),
            _ => None,
        }
    }
}

fn prec_climber() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::op_or, Assoc::Left),
        Operator::new(Rule::op_and, Assoc::Left),
        Operator::new(Rule::op_eq, Assoc::Left) | Operator::new(Rule::op_neq, Assoc::Left) |
            Operator::new(Rule::op_ge, Assoc::Left) | Operator::new(Rule::op_gt, Assoc::Left) |
            Operator::new(Rule::op_le, Assoc::Left) | Operator::new(Rule::op_lt, Assoc::Left),
        Operator::new(Rule::op_add, Assoc::Left) | Operator::new(Rule::op_sub, Assoc::Left),
        Operator::new(Rule::op_mul, Assoc::Left) | Operator::new(Rule::op_div, Assoc::Left),
        Operator::new(Rule::op_pow, Assoc::Right)
    ])
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    let mut file = File::open("scripts/arithmetic.ruo")?.read_to_string(&mut input)?;

    let ast = RuoParser::parse(Rule::program, &input).unwrap().next().unwrap();
    println!("Ast: {:#?}", ast);

    fn climb(expr: Pairs<Rule>) -> Box<Expr> {
        let primary = |pair: Pair<Rule>| {
            match pair.as_rule() {
                Rule::int => Box::new(Expr::Int(FromStr::from_str(pair.as_str()).unwrap())),
                Rule::float => Box::new(Expr::Float(FromStr::from_str(pair.as_str()).unwrap())),
                Rule::identifier => Box::new(Expr::Variable(pair.as_str().to_string())),
                Rule::bin_expr => climb(pair.into_inner()),
                _ => unreachable!()
            }
        };

        let infix = |lhs: Box<Expr>, op: Pair<Rule>, rhs: Box<Expr>| {
            if let Some(bin_op) = op.as_rule().to_bin_op() {
                Box::new(Expr::BinOp(lhs, bin_op, rhs))
            } else {
                panic!("not binop!");
            }
        };
        prec_climber().climb(expr, primary, infix)
    }


    println!("{:#?}", climb(ast.into_inner()));

    /*println!("Ast: {:?}", ast);

    let mut vm = vm::VM::new();
    vm.add_variable("print", Value::Function(print));
    vm.parse_ast(ast);
    vm.execute();

    println!("Variables: {:?}", vm.variables());*/

    Ok(())
}
