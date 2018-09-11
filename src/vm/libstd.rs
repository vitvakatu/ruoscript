use stack::Stack;
use value::Value;

pub fn print(stack: &mut Stack, arg_count: u8) {
    for _ in 0..arg_count {
        println!("{:?}", stack.pop());
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
