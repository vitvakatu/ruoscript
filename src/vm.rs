use ast::{BinOp, Expr, UnOp};
use stack::Stack;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
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
    Push {
        from: StorageVar,
    },
    Pop {
        to: StorageVar,
    },
    Label {
        label: Label,
    },
    Jmp {
        cond: StorageVar,
        to: Label,
    },
    JmpU {
        to: Label,
    },
    JmpReturn,
    LabelReturn,
    ScopeStart,
    ScopeEnd,
    NativeFunCall {
        func: StorageVar,
        arg_count: u8,
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

pub struct Storage {
    storage: Vec<Value>,
    last: LocalVar,
    stack: Stack,
    environment: Environment,
    native_functions: HashMap<Ident, StorageVar>,
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

fn print(stack: &mut Stack, arg_count: u8) {
    for _ in 0..arg_count {
        println!("{:?}", stack.pop());
    }
}

impl Storage {
    pub fn init_native_functions(&mut self) {
        let var = self.get_free();
        self.store(var.clone(), Value::Function(print));
        self.native_functions.insert("print".to_string(), var);
    }

    pub fn get_free(&mut self) -> StorageVar {
        let res = self.last;
        self.storage.push(Value::Int(0));
        self.last += 1;
        StorageVar::Local(res)
    }

    pub fn stack_mut(&mut self) -> &mut Stack {
        &mut self.stack
    }

    pub fn store(&mut self, var: StorageVar, value: Value) {
        match var {
            StorageVar::Local(local) => {
                self.storage[local] = value;
            }
            StorageVar::User(ident) => {
                let var = self.environment.borrow().get(ident.clone());
                let var = if let Some(var) = var {
                    var
                } else {
                    if let StorageVar::Local(var) = self.get_free() {
                        var
                    } else { unreachable!() }
                };
                self.environment.borrow_mut().set(ident.clone(), var);
                self.storage[var] = value;
            }
        }
    }

    pub fn load(&self, var: StorageVar) -> Value {
        match var {
            StorageVar::Local(local) => self.storage[local].clone(),
            StorageVar::User(ident) => {
                let var: LocalVar = self.environment.borrow().get(ident).unwrap();
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
enum Stacked {
    Command(Command),
    Expr((StorageVar, Box<Expr>)),
}

fn stacked_expr(var: StorageVar, expr: Box<Expr>) -> Stacked {
    Stacked::Expr((var, expr))
}

impl Command {
    pub fn commands_from_expr(expr: Box<Expr>, storage: &mut Storage) -> Vec<Command> {
        let mut stack: Vec<Stacked> = Vec::new();
        let mut commands: Vec<Command> = Vec::new();
        let mut label = 0;
        stack.push(stacked_expr(storage.get_free(), expr));

        while let Some(stacked) = stack.pop() {
            let (res_var, expr) = match stacked {
                Stacked::Expr((var, expr)) => (var, expr),
                Stacked::Command(command) => {
                    commands.push(command);
                    continue;
                }
            };
            match { *expr } {
                Expr::Block(exprs) => {
                    stack.push(Stacked::Command(Command::ScopeStart));
                    let mut last_var = storage.get_free();
                    stack.extend(exprs.iter().map(|e| {
                        last_var = storage.get_free();
                        stacked_expr(last_var.clone(), e.clone())
                    }));
                    stack.push(Stacked::Command(Command::Move {
                        from: last_var,
                        to: res_var,
                    }));
                    stack.push(Stacked::Command(Command::ScopeEnd));
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
                    stack.push(stacked_expr(var.clone(), expr.clone()));
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
                Expr::FunCall(ident, args) => {
                    if let Some(func) = storage.get_native_function(ident.clone()) {
                        let mut arg_count = 0;
                        for arg in args {
                            let var = storage.get_free();
                            stack.push(stacked_expr(var.clone(), arg.clone()));
                            stack.push(Stacked::Command(Command::Push { from: var }));
                            arg_count += 1;
                        }
                        commands.push(Command::NativeFunCall {
                            func,
                            arg_count,
                            result: res_var,
                        });
                    } else {
                        for arg in args {
                            let var = storage.get_free();
                            stack.push(stacked_expr(var.clone(), arg.clone()));
                            stack.push(Stacked::Command(Command::Push { from: var }));
                        }
                        stack.push(Stacked::Command(Command::LabelReturn));
                        stack.push(Stacked::Command(Command::JmpU {
                            to: Label::Function(ident.clone(), false),
                        }));
                        stack.push(Stacked::Command(Command::Pop { to: res_var }));
                    }
                }
                Expr::FunDecl(ident, args, body) => {
                    let end_label = Label::Direct(label);
                    label += 1;
                    stack.push(Stacked::Command(Command::JmpU {
                        to: end_label.clone(),
                    }));
                    stack.push(Stacked::Command(Command::Label {
                        label: Label::Function(ident, true),
                    }));
                    stack.push(Stacked::Command(Command::ScopeStart));
                    for arg in args {
                        stack.push(Stacked::Command(Command::Pop {
                            to: StorageVar::User(arg.clone()),
                        }));
                    }
                    stack.push(stacked_expr(res_var.clone(), body.clone()));
                    stack.push(Stacked::Command(Command::Push {
                        from: res_var.clone(),
                    }));
                    stack.push(Stacked::Command(Command::ScopeEnd));
                    stack.push(Stacked::Command(Command::JmpReturn));
                    stack.push(Stacked::Command(Command::Label {
                        label: end_label.clone(),
                    }));
                }
                Expr::UnOp(op, value) => {
                    let var = storage.get_free();
                    stack.push(stacked_expr(var.clone(), value.clone()));
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
                    stack.push(stacked_expr(left_var.clone(), left.clone()));
                    stack.push(stacked_expr(right_var.clone(), right.clone()));
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
                Expr::WhileLoop(cond, code) => {
                    // Label<start_label> Store<cond> Jmp<end_label> Block<code> Jmp<start_label> Label<end_label>
                    // this should be in stack
                    let cond_var = storage.get_free();
                    let label_start = label;
                    label += 1;
                    let label_end = label;
                    label += 1;
                    stack.push(Stacked::Command(Command::Label {
                        label: Label::Direct(label_start),
                    }));
                    stack.push(stacked_expr(cond_var.clone(), cond.clone()));
                    stack.push(Stacked::Command(Command::Jmp {
                        to: Label::Direct(label_end),
                        cond: cond_var,
                    }));
                    stack.push(stacked_expr(storage.get_free(), code.clone()));
                    stack.push(Stacked::Command(Command::JmpU {
                        to: Label::Direct(label_start),
                    }));
                    stack.push(Stacked::Command(Command::Label {
                        label: Label::Direct(label_end),
                    }));
                }
                Expr::If(cond, pos, neg) => {
                    // Label<1> BlockFalse Label<0> JmpU<1> BlockTrue Jmp<0> Cond
                    // ^ this should be in commands
                    let cond_var = storage.get_free();
                    let label_false = label;
                    label += 1;
                    let label_true = label;
                    label += 1;
                    stack.push(stacked_expr(cond_var.clone(), cond.clone()));
                    stack.push(Stacked::Command(Command::Jmp {
                        cond: cond_var,
                        to: Label::Direct(label_false),
                    }));
                    stack.push(stacked_expr(storage.get_free(), pos.clone()));
                    stack.push(Stacked::Command(Command::JmpU {
                        to: Label::Direct(label_true),
                    }));
                    stack.push(Stacked::Command(Command::Label {
                        label: Label::Direct(label_false),
                    }));
                    stack.push(stacked_expr(storage.get_free(), neg.clone()));
                    stack.push(Stacked::Command(Command::Label {
                        label: Label::Direct(label_true),
                    }));
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
    return_stack: Vec<usize>,
    storage: Storage,
}

impl VM {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear_commands(&mut self) {
        self.commands.clear();
        self.current_command = 0;
        self.return_stack.clear()
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
        let mut labels: HashMap<Label, usize> = HashMap::new();
        for (index, command) in self.commands.iter_mut().enumerate() {
            if let Command::Label { ref label } = command {
                match label {
                    Label::Function(ident, true) => {
                        labels.insert(label.clone(), index);
                        labels.insert(Label::Function(ident.clone(), false), index);
                    }
                    Label::Direct(_) => {
                        labels.insert(label.clone(), index);
                    }
                    _ => {}
                }
            }
        }
        while let Some(command) = self.commands.get(self.current_command).cloned() {
            match command {
                Command::Store { to, value } => {
                    self.store(to, value);
                }
                Command::Move { to, from } => {
                    let value = self.load(from);
                    self.store(to, value);
                }
                Command::NativeFunCall {
                    func,
                    arg_count,
                    result,
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
                    continue;
                }
                Command::Label { .. } => {}
                Command::LabelReturn => {
                    self.return_stack.push(self.current_command + 2);
                }
                Command::ScopeStart => {
                    self.storage.scope_start();
                }
                Command::ScopeEnd => {
                    self.storage.scope_end();
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
                Command::Halt => break,
            }
            self.current_command += 1;
        }
        Ok(())
    }
}
