use super::storage::Storage;
use super::storage::StorageVar;
use ast::BinOp;
use ast::CmpOp;
use ast::Expr;
use ast::UnOp;
use types::*;
use value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    Store {
        to: StorageVar,
        value: Value,
    },
    DeclareVar {
        ident: Ident,
        value: StorageVar,
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
}

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
                Expr::String(value) => {
                    let command = Command::Store {
                        to: res_var,
                        value: Value::String(value.clone()),
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
                Expr::DeclareVar(ident, expr) => {
                    let var = storage.get_free();
                    stack.push(stacked_expr(var.clone(), expr.clone()));
                    commands.push(Command::DeclareVar {
                        ident: ident.clone(),
                        value: var,
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
                        for arg in args.iter().rev() {
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
                        for arg in args.iter().rev() {
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
                        let var = storage.get_free();
                        stack.push(Stacked::Command(Command::Pop { to: var.clone() }));
                        stack.push(Stacked::Command(Command::DeclareVar {
                            ident: arg.clone(),
                            value: var.clone(),
                        }));
                    }
                    stack.push(stacked_expr(res_var.clone(), body.clone()));
                    stack.push(Stacked::Command(Command::Push {
                        from: res_var.clone(),
                    }));
                    stack.push(Stacked::Command(Command::JmpReturn));
                    stack.push(Stacked::Command(Command::ScopeEnd));
                    stack.push(Stacked::Command(Command::Label {
                        label: end_label.clone(),
                    }));
                }
                Expr::Return(expr) => {
                    let var = storage.get_free();
                    stack.push(stacked_expr(var.clone(), expr.clone()));
                    stack.push(Stacked::Command(Command::Push { from: var.clone() }));
                    stack.push(Stacked::Command(Command::JmpReturn));
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
                    // Cond Jmp<0> BlockTrue JmpU<1> Label<0> BlockFalse Label<1>
                    // ^ this should be in stack
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
