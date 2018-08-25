use ast::{BinOp, Expr, UnOp};
use std::collections::HashMap;
use types::*;
use value::Value;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmpOp {
    Lt,
    Le,
    Gt,
    Ge,
    NotEq,
    Eq,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Command {
    StoreInt {
        to: LocalVar,
        value: Int,
    },
    StoreFloat {
        to: LocalVar,
        value: Float,
    },
    StoreBool {
        to: LocalVar,
        value: Bool,
    },
    Assign {
        to: LocalVar,
        from: LocalVar,
    },
    FunCall {
        func: LocalVar,
        arg: LocalVar,
    },
    Add {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    Sub {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    Mul {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    Div {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    Pow {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    UnaryMinus {
        value: LocalVar,
        result: LocalVar,
    },
    UnaryNot {
        value: LocalVar,
        result: LocalVar,
    },
    Cmp {
        left: LocalVar,
        right: LocalVar,
        cmp: CmpOp,
        result: LocalVar,
    },
    And {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    Or {
        left: LocalVar,
        right: LocalVar,
        result: LocalVar,
    },
    Halt,
}

#[derive(Debug, Default)]
struct VariableInfo {
    pub ident: String,
    pub var: LocalVar,
}

#[derive(Default)]
pub struct Storage {
    storage: Vec<Value>,
    last: LocalVar,
    variables: HashMap<String, LocalVar>,
}

impl Storage {
    pub fn get_free(&mut self) -> LocalVar {
        let res = self.last;
        self.storage.push(Value::Int(0));
        self.last += 1;
        res
    }

    pub fn add_variable(&mut self, ident: String, var: LocalVar) {
        self.variables.insert(ident, var);
    }

    pub fn get_variable(&self, ident: &str) -> Option<LocalVar> {
        self.variables.get(ident).cloned()
    }

    pub fn variables(&self) -> Vec<(String, Value)> {
        self.variables
            .iter()
            .map(|(k, v)| (k.clone(), self.load(*v)))
            .collect()
    }

    pub fn store(&mut self, var: LocalVar, value: Value) {
        self.storage[var] = value;
    }

    pub fn load(&self, var: LocalVar) -> Value {
        self.storage[var].clone()
    }
}

/*
3 + 5
BinOp<1, Add, 2>
1. (#0 BinOp)     c:
2. (#1 3) (#2 5)  c: Add<#1 #2 #0>
3. (#1 3)         c: Add<#1 #2 #0> Store<#2 5>
4.                c: Add<#1 #2 #0> Store<#2 5> Store<#1 3>
Result:
Store<1 3>
Store<2 5>
Add<1 2 0>

3 * (1 + 2)
BinOp<3, Mul, BinOp<1, Add, 2>>
1. (#0 BinOp<Mul>)            c:
2. (#1 3) (#2 BinOp<Add>)     c: Mul<#1 #2 #0>
3. (#1 3) (#3 1) (#4 2)       c: Mul<#1 #2 #0> Add<#3 #4 #2>
4.                            c: Mul<#1 #2 #0> Add<#3 #4 #2> Store<#4 2> Store<#3 1> Store<#1 3>
Result:
Store #1 3
Store #3 1
Store #4 2
Add #3 #4 #2
Mul #1 #2 #0
*/

impl Command {
    pub fn commands_from_expr(expr: Box<Expr>, storage: &mut Storage) -> Vec<Command> {
        let mut stack = Vec::new();
        let mut commands = Vec::new();
        stack.push((storage.get_free(), expr));

        while let Some((res_var, expr)) = stack.pop() {
            match { *expr } {
                Expr::Int(value) => {
                    let command = Command::StoreInt { to: res_var, value };
                    commands.push(command);
                }
                Expr::Float(value) => {
                    let command = Command::StoreFloat { to: res_var, value };
                    commands.push(command);
                }
                Expr::Bool(value) => {
                    let command = Command::StoreBool { to: res_var, value };
                    commands.push(command);
                }
                Expr::Assign(ident, expr) => {
                    let var = storage.get_free();
                    stack.push((var, expr.clone()));
                    storage.add_variable(ident, var);
                    commands.push(Command::Assign {
                        to: res_var,
                        from: var,
                    });
                }
                Expr::Variable(ident) => {
                    let var = storage.get_variable(&ident).unwrap();
                    commands.push(Command::Assign {
                        to: res_var,
                        from: var,
                    });
                }
                Expr::FunCall(ident, arg) => {
                    let func = storage.get_variable(&ident).unwrap();
                    let var = storage.get_free();
                    stack.push((var, arg.clone()));
                    commands.push(Command::FunCall {
                        func,
                        arg: var,
                    });
                }
                Expr::UnOp(op, value) => {
                    let var = storage.get_free();
                    stack.push((var, value.clone()));
                    let command = match op {
                        UnOp::Not => Command::UnaryNot {
                            value: var,
                            result: res_var,
                        },
                        UnOp::Minus => Command::UnaryMinus {
                            value: var,
                            result: res_var,
                        },
                    };
                    commands.push(command);
                }
                Expr::BinOp(left, op, right) => {
                    let left_var = storage.get_free();
                    let right_var = storage.get_free();
                    stack.push((left_var, left.clone()));
                    stack.push((right_var, right.clone()));
                    let command = match op {
                        BinOp::Add => Command::Add {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::Sub => Command::Sub {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::Mul => Command::Mul {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::Div => Command::Div {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::Pow => Command::Pow {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::And => Command::And {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::Or => Command::Or {
                            left: left_var,
                            right: right_var,
                            result: res_var,
                        },
                        BinOp::Lt => Command::Cmp {
                            left: left_var,
                            right: right_var,
                            cmp: CmpOp::Lt,
                            result: res_var,
                        },
                        BinOp::Le => Command::Cmp {
                            left: left_var,
                            right: right_var,
                            cmp: CmpOp::Le,
                            result: res_var,
                        },
                        BinOp::Gt => Command::Cmp {
                            left: left_var,
                            right: right_var,
                            cmp: CmpOp::Gt,
                            result: res_var,
                        },
                        BinOp::Ge => Command::Cmp {
                            left: left_var,
                            right: right_var,
                            cmp: CmpOp::Ge,
                            result: res_var,
                        },
                        BinOp::NotEq => Command::Cmp {
                            left: left_var,
                            right: right_var,
                            cmp: CmpOp::NotEq,
                            result: res_var,
                        },
                        BinOp::Eq => Command::Cmp {
                            left: left_var,
                            right: right_var,
                            cmp: CmpOp::Eq,
                            result: res_var,
                        },
                    };
                    commands.push(command);
                }
            }
        }

        commands.reverse();
        commands
    }
}

#[derive(Debug, Clone)]
pub enum ExecutionError {
    InvalidType { expected: String, got: String },
}

#[derive(Default)]
pub struct VM {
    commands: Vec<Command>,
    current_command: usize,
    storage: Storage,
}

impl VM {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear_commands(&mut self) {
        self.commands.clear();
        self.current_command = 0;
    }

    pub fn add_variable(&mut self, ident: &str, value: Value) {
        let var = self.storage.get_free();
        self.store(var, value);
        self.storage.add_variable(ident.to_string(), var);
    }

    pub fn variables(&self) -> Vec<(String, Value)> {
        self.storage.variables()
    }

    pub fn parse_ast(&mut self, ast: Box<Expr>) {
        self.commands
            .extend(&Command::commands_from_expr(ast, &mut self.storage));

        println!("Commands received: {:?}", self.commands);
    }

    pub fn store(&mut self, var: LocalVar, value: Value) {
        self.storage.store(var, value);
    }

    pub fn load(&mut self, var: LocalVar) -> Value {
        self.storage.load(var)
    }

    pub fn execute(&mut self) -> Result<Value, ExecutionError> {
        while let Some(command) = self.commands.get(self.current_command).cloned() {
            match command {
                Command::StoreInt { to, value } => {
                    self.store(to, Value::Int(value));
                }
                Command::StoreFloat { to, value } => {
                    self.store(to, Value::Float(value));
                }
                Command::StoreBool { to, value } => {
                    self.store(to, Value::Bool(value));
                }
                Command::Assign { to, from } => {
                    let value = self.load(from);
                    self.store(to, value);
                }
                Command::FunCall { func, arg } => {
                    let function = self.load(func);
                    let arg = self.load(arg);
                    match function {
                        Value::Function(f) => {
                            f(arg)
                        }
                        _ => unreachable!()
                    }
                }
                Command::UnaryMinus { value, result } => {
                    let value = self.load(value);
                    match value {
                        Value::Int(i) => self.store(result, Value::Int(-i)),
                        Value::Float(f) => self.store(result, Value::Float(-f)),
                        _ => unreachable!(),
                    }
                }
                Command::UnaryNot { value, result } => {
                    let value = self.load(value);
                    match value {
                        Value::Bool(b) => self.store(result, Value::Bool(!b)),
                        _ => unreachable!(),
                    }
                }
                Command::Add {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.store(result, Value::Int(left + right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.store(result, Value::Float(left + right));
                        }
                        (f, s) => {
                            return Err(ExecutionError::InvalidType {
                                expected: format!("Equal types: {:?}, {:?}", f, s),
                                got: format!("Different types: {:?}, {:?}", f, s),
                            })
                        }
                    }
                }
                Command::Sub {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.store(result, Value::Int(left - right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.store(result, Value::Float(left - right));
                        }
                        (f, s) => {
                            return Err(ExecutionError::InvalidType {
                                expected: format!("Equal types: {:?}, {:?}", f, s),
                                got: format!("Different types: {:?}, {:?}", f, s),
                            })
                        }
                    }
                }
                Command::Mul {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.store(result, Value::Int(left * right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.store(result, Value::Float(left * right));
                        }
                        (f, s) => {
                            return Err(ExecutionError::InvalidType {
                                expected: format!("Equal types: {:?}, {:?}", f, s),
                                got: format!("Different types: {:?}, {:?}", f, s),
                            })
                        }
                    }
                }
                Command::Div {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.store(result, Value::Int(left / right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.store(result, Value::Float(left / right));
                        }
                        (f, s) => {
                            return Err(ExecutionError::InvalidType {
                                expected: format!("Equal types: {:?}, {:?}", f, s),
                                got: format!("Different types: {:?}, {:?}", f, s),
                            })
                        }
                    }
                }
                Command::Pow {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.store(result, Value::Int(left.pow(right as u32) as i64));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.store(result, Value::Float(left.powf(right)));
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            self.store(result, Value::Float(left.powi(right as i32)))
                        }
                        (f, s) => {
                            return Err(ExecutionError::InvalidType {
                                expected: "(Int Int) or (Float Float) or (Float Int)".into(),
                                got: format!("({:?} {:?})", f, s),
                            })
                        }
                    }
                }
                Command::Cmp {
                    left,
                    right,
                    cmp,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    let result_value = match cmp {
                        CmpOp::Lt => left < right,
                        CmpOp::Le => left <= right,
                        CmpOp::Gt => left > right,
                        CmpOp::Ge => left >= right,
                        CmpOp::NotEq => left != right,
                        CmpOp::Eq => left == right,
                    };
                    self.store(result, Value::Bool(result_value));
                }
                Command::And {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Bool(l), Value::Bool(r)) => self.store(result, Value::Bool(l && r)),
                        _ => unreachable!(),
                    }
                }
                Command::Or {
                    left,
                    right,
                    result,
                } => {
                    let (left, right) = (self.load(left), self.load(right));
                    match (left, right) {
                        (Value::Bool(l), Value::Bool(r)) => self.store(result, Value::Bool(l || r)),
                        _ => unreachable!(),
                    }
                }
                Command::Halt => break,
            }
            self.current_command += 1;
        }
        Ok(self.load(0))
    }
}
