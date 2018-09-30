use codegen::Context;
use llvm_sys::{core, execution_engine::*};

pub fn execute_main(context: &mut Context) -> i32 {
    unsafe {
        LLVMLinkInMCJIT();
        let mut execution_engine = 0 as LLVMExecutionEngineRef;
        let mut err = 0 as *mut i8;
        if LLVMCreateExecutionEngineForModule(&mut execution_engine, context.module, &mut err) != 0
        {
            panic!("Could not create execution engine for module");
        }

        let mut args: Vec<LLVMGenericValueRef> = Vec::new();

        let main_function =
            core::LLVMGetNamedFunction(context.module, b"main\0".as_ptr() as *const _);
        if main_function.is_null() {
            panic!("Could not find main function");
        }

        let mut arg_v = Vec::new();
        let mut env_v = Vec::new();
        let ret = LLVMRunFunctionAsMain(
            execution_engine,
            main_function,
            0 as _,
            arg_v.as_ptr(),
            env_v.as_ptr(),
        );
        ret
    }
}
