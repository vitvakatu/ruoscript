use pest::{Parser as PestParser, iterators::Pair};

use std::{fs::File, io::{self, Read}};

use ast::{Expr, BinOp};
use prec_climber::{tokenize, Parser};

const _GRAMMAR: &str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct RuoParser;

pub fn to_ast(pair: Pair<Rule>) -> Box<Expr> {
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
            Box::new(Expr::DeclareVar(ident, to_ast(arg)))
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
        Rule::for_loop => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_string();
            let lexpr = to_ast(inner.next().unwrap());
            let rexpr = to_ast(inner.next().unwrap());
            let body = to_ast(inner.next().unwrap());
            let for_loop = Box::new(Expr::Block(vec![
                Box::new(Expr::DeclareVar(ident.clone(), lexpr)),
                Box::new(Expr::WhileLoop(
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Variable(ident.clone())),
                        BinOp::Lt,
                        rexpr,
                    )),
                    Box::new(Expr::Block(vec![
                        body,
                        Box::new(Expr::Assign(
                            ident.clone(),
                            Box::new(Expr::BinOp(
                                Box::new(Expr::Variable(ident.clone())),
                                BinOp::Add,
                                Box::new(Expr::Int(1)),
                            )),
                        )),
                    ])),
                )),
            ]));
            for_loop
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
        Rule::return_stmt => Box::new(Expr::Return(to_ast(pair.into_inner().next().unwrap()))),
        _ => unreachable!(),
    }
}

pub fn parse_file(filename: &str) -> io::Result<Box<Expr>> {
    let mut input = String::new();
    File::open(filename)?.read_to_string(&mut input)?;

    Ok(parse_string(&input))
}

pub fn parse_string(string: &str) -> Box<Expr> {
    let ast = RuoParser::parse(Rule::program, string)
        .unwrap()
        .next()
        .unwrap();

    let ast = to_ast(ast.into_inner().next().unwrap());
    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn empty() {
        assert_eq!(parse_string(""), Box::new(Expr::Block(vec![])));
    }
}
