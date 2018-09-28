#[macro_use]
extern crate log;
extern crate env_logger;
extern crate failure;
#[macro_use]
extern crate failure_derive;

mod ast;
mod climber;
//mod parser;
mod lexer;
mod stack;
mod types;
mod value;

use std::env;
use std::fs::File;
use std::io::{self, Write};
use std::process::Command;

use ast::Emit;
use ast::Expr;

//use parser::parse_string;

fn make_executable(code: &str) -> String {
    let mut src = String::new();
    src.push_str("#include <stdio.h>\n");
    src.push_str("int main() {\n");
    src.push_str(code);
    src.push_str("return 0;\n");
    src.push_str("}");
    src
}

fn main() -> io::Result<()> {
    /*let mut src = tempfile::Builder::new().suffix(".c").tempfile()?;

    let code = "a := 34*3-2\nreturn a";

    let ast = parse_string(code).unwrap();

    println!("Ast: {:?}", ast);

    let src_code = make_executable(&ast.emit());

    println!("Generated code: \n{}", src_code);

    let bytes_written = src.write(src_code.as_bytes())?;
    println!("Written {} bytes", bytes_written);

    let mut command = Command::new("gcc")
        .arg("-o")
        .arg(::std::path::Path::new("executable"))
        .arg(src.path())
        .spawn()?;

    command.wait()?;*/
    Ok(())
}
