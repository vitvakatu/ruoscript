use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use types::*;

pub type Environment = Rc<RefCell<EnvironmentData>>;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct EnvironmentData {
    variables: HashMap<String, LocalVar>,
    pub outer: Option<Environment>,
}

impl EnvironmentData {
    pub fn new(outer: Option<Environment>) -> Self {
        Self {
            outer,
            ..Default::default()
        }
    }

    pub fn top(env: &Environment) -> Environment {
        match env.borrow().outer {
            Some(ref e) => EnvironmentData::top(e),
            None => env.clone(),
        }
    }

    pub fn set(&mut self, ident: Ident, var: LocalVar) {
        self.variables.insert(ident, var);
    }

    pub fn find(&self, key: String) -> Option<Environment> {
        if self.variables.contains_key(&key) {
            Some(Rc::new(RefCell::new(self.clone())))
        } else {
            match self.outer {
                Some(ref outer) => {
                    let outer = outer.borrow();
                    outer.find(key)
                }
                None => None,
            }
        }
    }

    pub fn get(&self, key: String) -> Option<LocalVar> {
        match self.variables.get(&key) {
            Some(v) => Some(v.clone()),
            None => {
                if let Some(env) = self.find(key.clone()) {
                    env.borrow().get(key)
                } else {
                    None
                }
            }
        }
    }
}
