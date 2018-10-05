use llvm_sys::{prelude::*, core};
use super::Context;
use parser::ast::Expr;
use std::ffi::CString;
use std::collections::HashMap;

use super::types::*;
use types::{TYPE_ANY, TYPE_INTEGER};

pub const ANY_TAG_INTEGER: u8 = 0;
pub const ANY_TAG_STRING: u8 = 1;
pub const ANY_TAG_POINTER: u8 = 2;

pub struct Any {
    pub tag: u8,
    pub payload: i64,
}

impl Any {
    pub fn get_type(ctx: &mut Context) -> LLVMTypeRef {
        let mut elements = vec![
            int8_type(ctx), // tag
            int64_type(ctx), // payload
        ];

        unsafe { core::LLVMStructTypeInContext(ctx.context, elements.as_mut_ptr(), 2, false as _) }
    }

    pub fn int_to_any(ctx: &mut Context) {
        unsafe {
            let mut argument_types = vec![core::LLVMPointerType(Self::get_type(ctx), 0), int32_type(ctx)];
            let function_type = core::LLVMFunctionType(
                core::LLVMVoidType(),
                argument_types.as_mut_ptr(),
                2,
                false as _,
            );
            let function_name = CString::new("int2any").unwrap();
            let function =
                core::LLVMAddFunction(ctx.module, function_name.as_ptr(), function_type);
            core::LLVMSetLinkage(function, llvm_sys::LLVMLinkage::LLVMExternalLinkage);

            let basic_block = core::LLVMAppendBasicBlockInContext(
                ctx.context,
                function,
                b"entry\0".as_ptr() as *const _,
            );
            core::LLVMPositionBuilderAtEnd(ctx.builder, basic_block);

            let any_ptr = core::LLVMGetParam(function, 0);
            let int = core::LLVMGetParam(function, 1);

            let payload_name = CString::new("payload").unwrap();
            let payload = core::LLVMBuildSExt(ctx.builder, int, int64_type(ctx), payload_name.as_ptr());
            let payload_ptr = core::LLVMBuildStructGEP(ctx.builder, any_ptr, 1, b"ptr\0".as_ptr() as *const _);
            core::LLVMBuildStore(ctx.builder, payload, payload_ptr);
            core::LLVMBuildRetVoid(ctx.builder);

            debug!("Before optimizations:");
            core::LLVMDumpValue(function);

            /*llvm_sys::analysis::LLVMVerifyFunction(
                function,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
            );*/
        }
    }

    pub fn allocate(ctx: &mut Context, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { core::LLVMBuildAlloca(ctx.builder, Self::get_type(ctx), name.as_ptr()) }
    }

    pub fn store(&self, ctx: &mut Context, ptr: LLVMValueRef) {
        unsafe {
            let tag = core::LLVMConstInt(int8_type(ctx), self.tag as _, false as _);
            let payload = core::LLVMConstInt(int64_type(ctx), self.payload as _, false as _);
            let mut values = vec![tag, payload];
            let value = core::LLVMConstStructInContext(ctx.context, values.as_mut_ptr(), 2, false as _);
            core::LLVMBuildStore(ctx.builder, value, ptr);
        }
    }

    pub fn get_int(ctx: &mut Context, ptr: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            core::LLVMBuildStructGEP(ctx.builder, ptr, 1, name.as_ptr())
        }
    }
}
