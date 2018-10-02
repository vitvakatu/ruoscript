use llvm_sys::{self, core, prelude::*};

use std::collections::HashMap;
use std::ffi::CString;

use parser::ast::Expr;

pub struct Context {
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub module: LLVMModuleRef,
    pub named_values: HashMap<String, LLVMValueRef>,
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
                named_values: HashMap::new(),
            }
        }
    }

    pub fn codegen_module(&mut self, exprs: Vec<Box<Expr>>) {
        for expr in &exprs {
            expr.codegen(self);
        }

        let string = unsafe {
            ::std::ffi::CStr::from_ptr(::llvm_sys::core::LLVMPrintModuleToString(self.module))
        };
        debug!("Generated module: \n {}", string.to_str().unwrap());
    }
}

pub trait Codegen {
    fn codegen(&self, context: &mut Context) -> LLVMValueRef;
}

impl Codegen for Expr {
    fn codegen(&self, context: &mut Context) -> LLVMValueRef {
        debug!("codegen: {:?}", self);
        unsafe {
            match *self {
                Expr::Integer(i) => {
                    let integer_type = core::LLVMInt32TypeInContext(context.context);
                    core::LLVMConstInt(integer_type, i as _, 0 as LLVMBool)
                }
                Expr::Variable(ref name) => context.named_values.get(name).cloned().unwrap(),
                Expr::Call(ref name, ref args) => {
                    if is_operator(&name) {
                        let lhs = args[0].codegen(context);
                        let rhs = args[1].codegen(context);

                        match name.as_str() {
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
                        }
                    } else {
                        let function_name = CString::new(name.as_bytes()).unwrap();
                        let function = core::LLVMGetNamedFunction(context.module, function_name.as_ptr());
                        if function.is_null() {
                            panic!();
                        }
                        let args_count = core::LLVMCountParams(function);
                        if args_count != args.len() as _ {
                            panic!()
                        }
                        let mut arguments: Vec<_> =
                            args.iter().map(|a| a.codegen(context)).collect();
                        let res = core::LLVMBuildCall(
                            context.builder,
                            function,
                            arguments.as_mut_ptr(),
                            arguments.len() as _,
                            b"calltmp\0".as_ptr() as *const _,
                        );
                        res
                    }
                }
                Expr::Prototype(ref proto) => {
                    let mut argument_types: Vec<_> = (0..proto.args.len())
                        .map(|_| core::LLVMInt32TypeInContext(context.context))
                        .collect();
                    let function_type = core::LLVMFunctionType(
                        core::LLVMInt32TypeInContext(context.context),
                        argument_types.as_mut_ptr(),
                        proto.args.len() as _,
                        false as _,
                    );
                    let function_name = CString::new(proto.name.clone()).unwrap();
                    let function = core::LLVMAddFunction(
                        context.module,
                        function_name.as_ptr(),
                        function_type,
                    );
                    core::LLVMSetLinkage(function, llvm_sys::LLVMLinkage::LLVMExternalLinkage);
                    function
                }
                Expr::Function(ref proto, ref body) => {
                    let function_name = CString::new(proto.name.clone()).unwrap();
                    let mut function =
                        core::LLVMGetNamedFunction(context.module, function_name.as_ptr());
                    if function.is_null() {
                        function = Expr::Prototype(proto.clone()).codegen(context);
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
                    context.named_values.clear();
                    let mut params = Vec::with_capacity(proto.args.len());
                    for i in 0..proto.args.len() {
                        let param = core::LLVMGetParam(function, i as _);
                        params.push(param);
                    }
                    for (arg, arg_name) in params.iter().zip(proto.args.iter()) {
                        context.named_values.insert(arg_name.clone(), arg.clone());
                    }
                    let ret_value = body.codegen(context);
                    core::LLVMBuildRet(context.builder, ret_value);
                    if llvm_sys::analysis::LLVMVerifyFunction(
                        function,
                        llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                    ) == true as _
                    {
                        debug!("Something goes wrong");
                        //panic!()
                    }

                    let pass_manager = core::LLVMCreateFunctionPassManagerForModule(context.module);
                    use llvm_sys::transforms::scalar::*;
                    LLVMAddBasicAliasAnalysisPass(pass_manager);
                    LLVMAddInstructionCombiningPass(pass_manager);
                    LLVMAddReassociatePass(pass_manager);
                    LLVMAddGVNPass(pass_manager);
                    LLVMAddCFGSimplificationPass(pass_manager);
                    core::LLVMInitializeFunctionPassManager(pass_manager);
                    core::LLVMRunFunctionPassManager(pass_manager, function);

                    function
                }
            }
        }
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
