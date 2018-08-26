mod ast;
mod grammar;
mod types;
mod value;
mod vm;

use std::fs::File;
use std::io::{self, Read};

use value::Value;

fn print(value: Value) -> Value {
    println!("{:?}", value);
    Value::Empty
}

fn main() -> io::Result<()> {
    let mut file = File::open("scripts/arithmetic.ruo")?;
    let parser = grammar::ProgramParser::new();
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let ast = parser.parse(&input).unwrap();

    println!("Ast: {:?}", ast);

    let mut vm = vm::VM::new();
    vm.add_variable("print", Value::Function(print));
    vm.parse_ast(ast);
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
