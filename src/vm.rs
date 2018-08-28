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

#[derive(Debug, Clone, PartialEq)]
pub enum StorageVar {
    Local(LocalVar),
    User(Ident),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    Store {
        to: StorageVar,
        value: Value,
    },
    Move {
        to: StorageVar,
        from: StorageVar,
    },
    FunCall {
        func: StorageVar,
        arg: StorageVar,
        result: StorageVar,
    },
    Add {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    Sub {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    Mul {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    Div {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    Pow {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    UnaryMinus {
        value: StorageVar,
        result: StorageVar,
    },
    UnaryNot {
        value: StorageVar,
        result: StorageVar,
    },
    Cmp {
        left: StorageVar,
        right: StorageVar,
        cmp: CmpOp,
        result: StorageVar,
    },
    And {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    Or {
        left: StorageVar,
        right: StorageVar,
        result: StorageVar,
    },
    Nop,
    Halt,
}

#[derive(Default)]
pub struct Storage {
    storage: Vec<Value>,
    last: LocalVar,
    variables: HashMap<String, LocalVar>,
}

impl Storage {
    pub fn get_free(&mut self) -> StorageVar {
        let res = self.last;
        self.storage.push(Value::Int(0));
        self.last += 1;
        StorageVar::Local(res)
    }

    pub fn store(&mut self, var: StorageVar, value: Value) {
        match var {
            StorageVar::Local(local) => {
                self.storage[local] = value;
            }
            StorageVar::User(ident) => {
                if let StorageVar::Local(var) = self.get_free() {
                    self.storage[var] = value;
                    self.variables.insert(ident, var);
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn load(&self, var: StorageVar) -> Value {
        match var {
            StorageVar::Local(local) => self.storage[local].clone(),
            StorageVar::User(ident) => {
                let var: LocalVar = *self.variables.get(&ident).unwrap();
                self.storage[var].clone()
            }
        }
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

/*
let a = 1;
print(a)
Block[Assign<"a", 1>, FunCall<"print", Variable<"a">>]
1. (#1) Block[...]                      c:
2. (#2 Assign<...>) (#3 FunCall<...>)   c:
3. (#2 Assign<...>) (#4 Variable<"a">)  c: FunCall<#0, #4, #3>
4. (#2 Assign<...>)                     c: FunCall<#0, #4, #3> Assign<#?, #4>
5. (#5 1)                               c: FunCall<#0, #4, #3> Assign<#?, #4> Assign<#5, #?>
6.                                      c: FunCall<#0, #4, #3> Assign<#?, #4> Assign<#5, #?> Store<#5 1>

StoreInt<#5, 1>
Assign<#5, #?>
Assign<#?, #4>
FunCall<#0, #4, #3>
*/

/*
Conditions:
if <test> then <then_branch> else <else_branch> end
if x then y else z end

*/

impl Command {
    pub fn commands_from_expr(expr: Box<Expr>, storage: &mut Storage) -> Vec<Command> {
        let mut stack = Vec::new();
        let mut commands = Vec::new();
        stack.push((storage.get_free(), expr));

        while let Some((res_var, expr)) = stack.pop() {
            match { *expr } {
                Expr::Block(exprs) => {
                    stack.extend(exprs.iter().map(|e| (storage.get_free(), e.clone())));
                }
                Expr::Int(value) => {
                    let command = Command::Store {
                        to: res_var,
                        value: Value::Int(value),
                    };
                    commands.push(command);
                }
                Expr::Float(value) => {
                    let command = Command::Store {
                        to: res_var,
                        value: Value::Float(value),
                    };
                    commands.push(command);
                }
                Expr::Bool(value) => {
                    let command = Command::Store {
                        to: res_var,
                        value: Value::Bool(value),
                    };
                    commands.push(command);
                }
                Expr::Assign(ident, expr) => {
                    let var = storage.get_free();
                    stack.push((var.clone(), expr.clone()));
                    commands.push(Command::Move {
                        to: StorageVar::User(ident.clone()),
                        from: var,
                    });
                }
                Expr::Variable(ident) => {
                    commands.push(Command::Move {
                        to: res_var,
                        from: StorageVar::User(ident.clone()),
                    });
                }
                Expr::FunCall(ident, arg) => {
                    let func = StorageVar::User(ident.clone());
                    let var = storage.get_free();
                    stack.push((var.clone(), arg.clone()));
                    commands.push(Command::FunCall {
                        func,
                        arg: var,
                        result: res_var,
                    });
                }
                Expr::UnOp(op, value) => {
                    let var = storage.get_free();
                    stack.push((var.clone(), value.clone()));
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
                    stack.push((left_var.clone(), left.clone()));
                    stack.push((right_var.clone(), right.clone()));
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
                Expr::Empty => {
                    commands.push(Command::Nop);
                }
                Expr::If(_, _, _) => unimplemented!(),
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

    pub fn parse_ast(&mut self, ast: Box<Expr>) {
        self.commands
            .extend(Command::commands_from_expr(ast, &mut self.storage));

        println!("Byte code:");
        for command in &self.commands {
            println!("{:?}", command);
        }
        println!("# ------------------------- #");
    }

    pub fn store(&mut self, var: StorageVar, value: Value) {
        self.storage.store(var, value);
    }

    pub fn load(&mut self, var: StorageVar) -> Value {
        self.storage.load(var)
    }

    pub fn execute(&mut self) -> Result<(), ExecutionError> {
        while let Some(command) = self.commands.get(self.current_command).cloned() {
            match command {
                Command::Store { to, value } => {
                    self.store(to, value);
                }
                Command::Move { to, from } => {
                    let value = self.load(from);
                    self.store(to, value);
                }
                Command::FunCall { func, arg, result } => {
                    let function = self.load(func);
                    let arg = self.load(arg);
                    match function {
                        Value::Function(f) => {
                            self.store(result, f(arg));
                        }
                        _ => unreachable!(),
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
                Command::Nop => {}
                Command::Halt => break,
            }
            self.current_command += 1;
        }
        Ok(())
    }
}
