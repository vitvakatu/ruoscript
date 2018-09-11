use super::env::{Environment, EnvironmentData};
use stack::Stack;
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};
use types::*;
use value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum StorageVar {
    Local(LocalVar),
    User(Ident),
}

pub struct Storage {
    storage: Vec<Value>,
    last: LocalVar,
    stack: Stack,
    environment: Environment,
    native_functions: HashMap<Ident, StorageVar>,
}

impl fmt::Debug for Storage {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Storage")
            //.field("storage", &self.storage.iter().filter(|v| v != &&Value::Empty).collect::<Vec<_>>())
            .field("stack", &self.stack)
            //.field("env", &self.environment)
            .finish()
    }
}

impl Default for Storage {
    fn default() -> Self {
        let mut res = Self {
            storage: Vec::new(),
            last: 0,
            stack: Stack::default(),
            environment: Environment::default(),
            native_functions: HashMap::new(),
        };
        res.init_native_functions();
        res
    }
}

impl Storage {
    pub fn init_native_functions(&mut self) {
        use super::libstd::*;
        let mut add_function = |name: &str, func| {
            let var = self.get_free();
            self.store(var.clone(), Value::Function(func));
            self.native_functions.insert(name.to_string(), var);
        };
        add_function("print", print);
        add_function("int", int);
        add_function("float", float);
        add_function("str", string);
    }

    pub fn get_free(&mut self) -> StorageVar {
        let res = self.last;
        self.storage.push(Value::Empty);
        self.last += 1;
        StorageVar::Local(res)
    }

    pub fn stack_mut(&mut self) -> &mut Stack {
        &mut self.stack
    }

    pub fn declare_variable(&mut self, ident: Ident) -> StorageVar {
        let var = self.get_free();
        if let StorageVar::Local(var) = var {
            self.environment.borrow_mut().set(ident, var);
        }
        var
    }

    pub fn store(&mut self, var: StorageVar, value: Value) {
        match var {
            StorageVar::Local(local) => {
                self.storage[local] = value;
            }
            StorageVar::User(ident) => {
                if let Some(var) = self.environment.borrow().get(ident.clone()) {
                    self.storage[var] = value;
                } else {
                    panic!("Variable not found in scope: {}", ident);
                }
            }
        }
    }

    pub fn load(&self, var: StorageVar) -> Value {
        match var {
            StorageVar::Local(local) => self.storage[local].clone(),
            StorageVar::User(ident) => {
                let var: LocalVar = self
                    .environment
                    .borrow()
                    .get(ident.clone())
                    .expect(&format!("Variable not found: {}", ident));
                self.storage[var].clone()
            }
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop()
    }

    pub fn get_native_function(&self, ident: String) -> Option<StorageVar> {
        self.native_functions.get(&ident).cloned()
    }

    pub fn scope_start(&mut self) {
        let outer = Rc::clone(&self.environment);
        let data = EnvironmentData::new(Some(outer));
        self.environment = Rc::new(RefCell::new(data));
    }

    pub fn scope_end(&mut self) {
        if self.environment.borrow().outer.is_some() {
            let outer = Rc::clone(self.environment.borrow().outer.as_ref().unwrap());
            self.environment = outer;
        }
    }
}
