use llvm_sys::{prelude::*, core};
use super::Context;

pub fn int8_type(ctx: &mut Context) -> LLVMTypeRef {
    unsafe { core::LLVMInt8TypeInContext(ctx.context) }
}

pub fn int32_type(ctx: &mut Context) -> LLVMTypeRef {
    unsafe { core::LLVMInt32TypeInContext(ctx.context) }
}

pub fn int64_type(ctx: &mut Context) -> LLVMTypeRef {
    unsafe { core::LLVMInt64TypeInContext(ctx.context) }
}
