use stack::Stack;
use value::Value;

use std::io::{self, BufRead};

pub fn print(stack: &mut Stack, arg_count: u8) {
    for _ in 0..arg_count {
        print!("{:?}", stack.pop());
    }
    if arg_count != 0 {
        print!("\n");
    }
    stack.push(Value::Empty);
}

pub fn int(stack: &mut Stack, arg_count: u8) {
    assert_eq!(arg_count, 1);
    let value = match stack.pop() {
        Value::Float(f) => Value::Int(f.round() as i64),
        Value::String(s) => Value::Int(s.parse().unwrap()),
        Value::Int(i) => Value::Int(i),
        Value::Bool(b) if b => Value::Int(1),
        Value::Bool(b) if !b => Value::Int(0),
        _ => panic!("Invalid input type"),
    };
    stack.push(value);
}

pub fn float(stack: &mut Stack, arg_count: u8) {
    assert_eq!(arg_count, 1);
    let value = match stack.pop() {
        Value::Int(i) => Value::Float(i as f64),
        Value::String(s) => Value::Float(s.parse().unwrap()),
        Value::Float(f) => Value::Float(f),
        _ => panic!("Invalid input type"),
    };
    stack.push(value);
}

pub fn string(stack: &mut Stack, arg_count: u8) {
    assert_eq!(arg_count, 1);
    let s = format!("{:?}", stack.pop());
    stack.push(Value::String(s));
}

pub fn read_line(stack: &mut Stack, arg_count: u8) {
    assert_eq!(arg_count, 0);
    let stdin = io::stdin();
    let line = stdin.lock().lines().next().unwrap().unwrap();
    stack.push(Value::String(line));
}

pub fn mod_op(stack: &mut Stack, arg_count: u8) {
    assert_eq!(arg_count, 2);
    let number = stack.pop();
    let modulo = stack.pop();
    match (number, modulo) {
        (Value::Int(n), Value::Int(m)) => stack.push(Value::Int(n % m)),
        _ => panic!("Invalid types"),
    }
}

pub fn len(stack: &mut Stack, arg_count: u8) {
    assert_eq!(arg_count, 1);
    let value = match stack.pop() {
        Value::String(s) => s.len(),
        _ => panic!("Invalid type"),
    };
    stack.push(Value::Int(value as i64));
}
