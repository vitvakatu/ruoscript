use value::Value;

#[derive(Default, Debug)]
pub struct Stack {
    inner: Vec<Value>,
}

impl Stack {
    pub fn push(&mut self, value: Value) {
        self.inner.push(value);
    }

    pub fn pop(&mut self) -> Value {
        self.inner.pop().unwrap()
    }
}