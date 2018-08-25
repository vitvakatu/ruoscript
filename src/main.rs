mod ast;
mod grammar;
mod vm;
mod value;
mod types;

use std::io::{self, Read};
use std::fs::File;

fn main() -> io::Result<()> {
    let mut file = File::open("scripts/arithmetic.ruo")?;
    let parser = grammar::ProgramParser::new();
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let ast = parser.parse(&input).unwrap();

    println!("Ast: {:?}", ast);

    let mut vm = vm::VM::new();
    for expr in ast {
        vm.parse_ast(expr);
    }
    vm.execute();

    println!("Variables: {:?}", vm.variables());

    Ok(())
}

/*
fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut vm = vm::VM::new();
    for input in stdin.lock().lines() {
        vm.clear_commands();
        let input = input.unwrap();
        if input == "halt".to_string() {
            break;
        }

        let parser = grammar::ExprParser::new();
        let ast = parser.parse(&input).unwrap();
        println!("{:?}", ast);
        vm.parse_ast(ast);
        let _ = vm.execute();
        let variables = vm.variables();
        println!("Variables: {:?}", variables);
    }

    Ok(())
}
*/
