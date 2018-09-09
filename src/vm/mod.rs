use std::collections::HashMap;

use self::command::Command;
pub use self::storage::{Storage, StorageVar};
use ast::{CmpOp, Expr};
use types::*;
use value::Value;

mod command;
mod env;
mod libstd;
mod storage;

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
Blocks
let a = 1;
let b = 2;
Block[Assign<Var<a>, Int<1>>, Assign<Var<b>, Int<2>>]
1. Block[...]                       c:
2. Assign<Var<a>>, Assign<Var<b>>   c:
4.                                  c: Assign<Var<b>> Assign<Var<a>>
*/

/*
Conditions:
if <test> then <then_branch> else <else_branch> end
if true then print(1) else print(2) end;
Expected: Store<#3, true> Jmp<#3, 0> Store<#2, 1> NativeFunCall<print, #2> JmpU<1> Label<0> Store<#1, 2> FunCall<print, #1> Label<1>
Cond Jmp<0> BlockTrue JmpU<1> Label<0> BlockFalse Label<1>
Reversed: Label<1> BlockFalse Label<0> JmpU<1> BlockTrue Jmp<0> Cond <- this should be in commands
*/

/*
While loop:
while <cond> do <code> end
Expected:
Label<start_label>
Store<cond>
Jmp<end_label>
Block<code>
Jmp<start_label>
Label<end_label>
*/

/*
Fun decl + fun call
Declaration:
JmpU<end_label>
Label<fun_label>
Pop<arg_name>
...
Block<code>
JmpReturn
Label<end_label>

Call:
Push<local_arg_var>
...
LabelReturn
JmpU<fun_label>
*/

#[derive(Debug, Clone)]
pub enum ExecutionError {
    InvalidType { expected: String, got: String },
}

#[derive(Default)]
pub struct VM {
    commands: Vec<Command>,
    current_command: usize,
    return_stack: Vec<usize>,
    storage: Storage,
}

impl VM {
    pub fn new() -> Self {
        Self::default()
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

    pub fn declare_variable(&mut self, ident: Ident) -> StorageVar {
        self.storage.declare_variable(ident)
    }

    pub fn load(&mut self, var: StorageVar) -> Value {
        self.storage.load(var)
    }

    pub fn execute(&mut self) -> Result<(), ExecutionError> {
        let mut nesting_levels: HashMap<usize, usize> = HashMap::new();
        let mut nesting_level = 0;
        let mut labels: HashMap<Label, usize> = HashMap::new();
        for (index, command) in self.commands.iter_mut().enumerate() {
            match command {
                Command::Label { ref label } => match label {
                    Label::Function(ident, true) => {
                        labels.insert(label.clone(), index);
                        labels.insert(Label::Function(ident.clone(), false), index);
                    }
                    Label::Direct(_) => {
                        labels.insert(label.clone(), index);
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        while let Some(command) = self.commands.get(self.current_command).cloned() {
            match command {
                Command::Store { to, value } => {
                    self.store(to, value);
                }
                Command::DeclareVar { ident, value } => {
                    let variable = self.declare_variable(ident);
                    let value = self.load(value);
                    self.store(variable, value);
                }
                Command::Move { to, from } => {
                    let value = self.load(from);
                    self.store(to, value);
                }
                Command::NativeFunCall {
                    func,
                    arg_count,
                    result: _,
                } => {
                    let function = self.load(func);
                    match function {
                        Value::Function(f) => {
                            f(self.storage.stack_mut(), arg_count);
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
                Command::Jmp { to, cond } => {
                    if let Value::Bool(false) = self.load(cond) {
                        self.current_command = *labels.get(&to).unwrap();
                        continue;
                    }
                }
                Command::JmpU { to } => {
                    self.current_command = *labels.get(&to).unwrap();
                    continue;
                }
                Command::JmpReturn => {
                    let command = self.return_stack.pop().unwrap();
                    self.current_command = command;
                    let label_nesting_level = nesting_levels.get(&command).unwrap();
                    for _ in 0..(nesting_level - label_nesting_level) {
                        self.storage.scope_end();
                        nesting_level -= 1;
                    }
                    continue;
                }
                Command::Label { .. } => {}
                Command::LabelReturn => {
                    self.return_stack.push(self.current_command + 2);
                    nesting_levels.insert(self.current_command + 2, nesting_level);
                }
                Command::ScopeStart => {
                    self.storage.scope_start();
                    nesting_level += 1;
                }
                Command::ScopeEnd => {
                    self.storage.scope_end();
                    nesting_level -= 1;
                }
                Command::Push { from } => {
                    let value = self.storage.load(from);
                    self.storage.push(value);
                }
                Command::Pop { to } => {
                    let value = self.storage.pop();
                    self.storage.store(to, value);
                }
                Command::Nop => {}
            }
            self.current_command += 1;
        }
        Ok(())
    }
}
