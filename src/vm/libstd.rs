use stack::Stack;

pub fn print(stack: &mut Stack, arg_count: u8) {
    for _ in 0..arg_count {
        println!("{:?}", stack.pop());
    }
}
