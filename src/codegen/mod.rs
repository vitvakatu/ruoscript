use llvm_sys::{self, core, prelude::*};

use std::collections::{HashMap, HashSet};
use std::ffi::CString;

use parser::ast::{Block, Expr, Module, Prototype, Statement, TopLevelStatement};
use types::TypedExpr;

pub struct Context {
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub module: LLVMModuleRef,
    pub local_arguments: HashMap<String, LLVMValueRef>,
    pub local_variables: HashMap<String, LLVMValueRef>,
    pub has_return_statement: bool,
}

impl Context {
    pub fn new() -> Self {
        unsafe {
            if llvm_sys::target::LLVM_InitializeNativeTarget() != 0 {
                panic!()
            }
            if llvm_sys::target::LLVM_InitializeNativeAsmParser() != 0 {
                panic!()
            }
            if llvm_sys::target::LLVM_InitializeNativeAsmPrinter() != 0 {
                panic!()
            }
            let context = core::LLVMContextCreate();
            let builder = core::LLVMCreateBuilderInContext(context);
            let module = core::LLVMModuleCreateWithNameInContext(
                b"entrymodule\0".as_ptr() as *const _,
                context,
            );
            Self {
                context,
                module,
                builder,
                local_arguments: HashMap::new(),
                local_variables: HashMap::new(),
                has_return_statement: false,
            }
        }
    }

    pub unsafe fn codegen_module(&mut self, module: Module) {
        for statement in &module.0 {
            statement.codegen(self);
        }

        let string = unsafe {
            ::std::ffi::CStr::from_ptr(core::LLVMPrintModuleToString(self.module))
        };
        debug!("Generated module: \n {}", string.to_str().unwrap());
    }
}

pub trait Codegen {
    unsafe fn codegen(&self, context: &mut Context) -> Option<LLVMValueRef>;
}

impl Codegen for TopLevelStatement {
    unsafe fn codegen(&self, context: &mut Context) -> Option<LLVMValueRef> {
        match *self {
            TopLevelStatement::Prototype(ref proto) => proto.codegen(context),
            TopLevelStatement::Function(ref proto, ref block) => {
                context.has_return_statement = false;
                let function_name = CString::new(proto.name.clone()).unwrap();
                let mut function =
                    core::LLVMGetNamedFunction(context.module, function_name.as_ptr());
                if function.is_null() {
                    function = proto.codegen(context).unwrap();
                }
                if function.is_null() {
                    panic!()
                }
                let basic_block = core::LLVMAppendBasicBlockInContext(
                    context.context,
                    function,
                    b"entry\0".as_ptr() as *const _,
                );
                core::LLVMPositionBuilderAtEnd(context.builder, basic_block);
                context.local_arguments.clear();
                let mut params = Vec::with_capacity(proto.args.len());
                for i in 0..proto.args.len() {
                    let param = core::LLVMGetParam(function, i as _);
                    params.push(param);
                }
                for (arg, arg_name) in params.iter().zip(proto.args.iter()) {
                    context
                        .local_arguments
                        .insert(arg_name.clone(), arg.clone());
                }

                match *block {
                    Block::Void(ref statements) => {
                        let mut need_return = true;
                        for statement in statements.iter() {
                            if let Statement::Return(_) = *statement {
                                need_return = false;
                            }
                            // FIXME: error on expr
                            let _ = statement.codegen(context);
                        }
                        if need_return {
                            // FIXME: return void when types come
                            let value = Expr::Integer(0).codegen(context).unwrap();
                            core::LLVMBuildRet(context.builder, value);
                        }
                    }
                    Block::NonVoid(ref statements, ref expr) => {
                        for statement in statements.iter() {
                            // FIXME: error on expr
                            let _ = statement.codegen(context);
                        }
                        let value = expr.codegen(context).unwrap();
                        core::LLVMBuildRet(context.builder, value);
                    }
                }

                debug!("Before optimizations:");
                core::LLVMDumpValue(function);

                llvm_sys::analysis::LLVMVerifyFunction(
                    function,
                    llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
                );

                let pass_manager = core::LLVMCreateFunctionPassManagerForModule(context.module);
                use llvm_sys::transforms::scalar::*;
                LLVMAddBasicAliasAnalysisPass(pass_manager);
                LLVMAddInstructionCombiningPass(pass_manager);
                LLVMAddReassociatePass(pass_manager);
                LLVMAddGVNPass(pass_manager);
                LLVMAddCFGSimplificationPass(pass_manager);
                core::LLVMInitializeFunctionPassManager(pass_manager);
                core::LLVMRunFunctionPassManager(pass_manager, function);

                Some(function)
            }
        }
    }
}

impl Codegen for Statement {
    unsafe fn codegen(&self, context: &mut Context) -> Option<LLVMValueRef> {
        match *self {
            Statement::Expr(ref expr) => expr.codegen(context),
            Statement::VariableDeclaration(ref name, ref expr) => {
                store_variable(&name, &expr.expr, context);
                None
            }
            Statement::Return(ref expr) => {
                let value = expr.codegen(context).unwrap();
                core::LLVMBuildRet(context.builder, value);
                context.has_return_statement = true;
                None
            }
        }
    }
}

impl Codegen for TypedExpr {
    unsafe fn codegen(&self, context: &mut Context) -> Option<LLVMValueRef> {
        self.expr.codegen(context)
    }
}

impl Codegen for Expr {
    unsafe fn codegen(&self, context: &mut Context) -> Option<LLVMValueRef> {
        debug!("codegen: {:?}", self);
        unsafe {
            match *self {
                Expr::Integer(i) => {
                    let integer_type = core::LLVMInt32TypeInContext(context.context);
                    Some(core::LLVMConstInt(integer_type, i as _, 0 as LLVMBool))
                }
                Expr::Variable(ref name) => {
                    if let Some(variable) = context.local_variables.get(name) {
                        Some(core::LLVMBuildLoad(
                            context.builder,
                            variable.clone(),
                            b"tmpload\0".as_ptr() as *const _,
                        ))
                    } else {
                        context.local_arguments.get(name).cloned()
                    }
                }
                Expr::Call(ref name, ref args) => {
                    if is_operator(&name) {
                        let lhs = args[0].codegen(context).unwrap();
                        let rhs = args[1].codegen(context).unwrap();

                        Some(match name.as_str() {
                            "+" => core::LLVMBuildAdd(
                                context.builder,
                                lhs,
                                rhs,
                                b"addtmp\0".as_ptr() as *const _,
                            ),
                            "-" => core::LLVMBuildSub(
                                context.builder,
                                lhs,
                                rhs,
                                b"subtmp\0".as_ptr() as *const _,
                            ),
                            "*" => core::LLVMBuildMul(
                                context.builder,
                                lhs,
                                rhs,
                                b"multmp\0".as_ptr() as *const _,
                            ),
                            "/" => core::LLVMBuildUDiv(
                                context.builder,
                                lhs,
                                rhs,
                                b"divtmp\0".as_ptr() as *const _,
                            ),
                            _ => unimplemented!(),
                        })
                    } else {
                        let function_name = CString::new(name.as_bytes()).unwrap();
                        let function =
                            core::LLVMGetNamedFunction(context.module, function_name.as_ptr());
                        if function.is_null() {
                            panic!();
                        }
                        let args_count = core::LLVMCountParams(function);
                        if args_count != args.len() as _ {
                            panic!()
                        }
                        let mut arguments: Vec<_> =
                            args.iter().map(|a| a.codegen(context).unwrap()).collect();
                        let res = core::LLVMBuildCall(
                            context.builder,
                            function,
                            arguments.as_mut_ptr(),
                            arguments.len() as _,
                            b"calltmp\0".as_ptr() as *const _,
                        );
                        Some(res)
                    }
                }
                _ => unimplemented!(),
            }
        }
    }
}

unsafe fn store_variable(name: &str, expr: &Expr, context: &mut Context) {
    let integer_type = core::LLVMInt32TypeInContext(context.context);
    let c_name = CString::new(name.as_bytes()).unwrap();
    let alloca = core::LLVMBuildAlloca(context.builder, integer_type, c_name.as_ptr());
    let init_expr = expr.codegen(context).unwrap();
    core::LLVMBuildStore(context.builder, init_expr, alloca);
    context.local_variables.insert(name.to_string(), alloca);
}

impl Codegen for Prototype {
    unsafe fn codegen(&self, context: &mut Context) -> Option<LLVMValueRef> {
        unsafe {
            let mut argument_types: Vec<_> = (0..self.args.len())
                .map(|_| core::LLVMInt32TypeInContext(context.context))
                .collect();
            let function_type = core::LLVMFunctionType(
                core::LLVMInt32TypeInContext(context.context),
                argument_types.as_mut_ptr(),
                self.args.len() as _,
                false as _,
            );
            let function_name = CString::new(self.name.clone()).unwrap();
            let function =
                core::LLVMAddFunction(context.module, function_name.as_ptr(), function_type);
            core::LLVMSetLinkage(function, llvm_sys::LLVMLinkage::LLVMExternalLinkage);
            Some(function)
        }
    }
}

unsafe fn get_any_type(ctx: &mut Context) -> LLVMTypeRef {
    let mut elements = vec![
        core::LLVMInt8TypeInContext(ctx.context), // tag
        core::LLVMInt64TypeInContext(ctx.context), // payload
    ];

    core::LLVMStructTypeInContext(ctx.context, elements.as_mut_ptr(), 2, false as _)
}

macro_rules! c_str {
    ($str:expr) => {
        let string = CString::new(s).unwrap();
        string.as_ptr()
    }
}

fn is_operator(name: &str) -> bool {
    match name {
        "^" => true,
        "!" => true,
        "/" => true,
        "*" => true,
        "+" => true,
        "-" => true,
        "==" => true,
        ">=" => true,
        "<=" => true,
        "!=" => true,
        "<" => true,
        ">" => true,
        "&&" => true,
        "||" => true,
        _ => false,
    }
}
