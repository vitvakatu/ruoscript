#[macro_use]
extern crate log;
extern crate env_logger;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate llvm_sys;
extern crate libc;

mod codegen;
mod executor;
mod lexer;
mod parser;
mod stack;
mod types;
mod value;

use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::process::Command;

#[no_mangle]
pub extern fn put_char(c: i32) -> i32 {
    print!("{}", c as u8 as char);
    0
}

fn main() -> io::Result<()> {
    env_logger::init();

    let mut args = env::args();
    args.next();
    let file_name = args.next().unwrap();

    println!("Reading {}...", file_name);

    let mut src = String::new();
    let _ = File::open(file_name)?.read_to_string(&mut src)?;

    let mut lexer = lexer::Lexer::new(src.char_indices());
    let tokens: Vec<_> = lexer.get_tokens().unwrap();
    let mut parser = parser::Parser::new(tokens.iter());

    unsafe {
        llvm_sys::support::LLVMLoadLibraryPermanently(::std::ptr::null());
        llvm_sys::support::LLVMAddSymbol(b"put_char\0".as_ptr() as *const _, put_char as *mut _);
    }

    let mut context = codegen::Context::new();

    let executor = executor::Executor::new(&mut context);

    parser.parse(&mut context).unwrap();

    let string =
        unsafe { ::std::ffi::CStr::from_ptr(::llvm_sys::core::LLVMPrintModuleToString(context.module)) };
    debug!("Final output: \n{}", string.to_str().unwrap());

    let ret = executor.execute_main(&mut context);
    println!("Returned: {}", ret);

    Ok(())
}
