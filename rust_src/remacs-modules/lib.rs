extern crate libc;
extern crate remacs_sys;

use remacs_sys::{emacs_env, emacs_runtime, emacs_value};
use std::ffi::CString;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr;
use std::slice;
use std::ffi::{IntoStringError, NulError};

#[repr(C)]
pub struct EmacsRuntime(*mut emacs_runtime);

#[repr(C)]
pub struct EmacsEnv(*mut emacs_env);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct EmacsValue(emacs_value);

#[repr(C)]
#[derive(Debug)]
pub enum EmacsModuleError {
    InvalidValue,
    NulError(NulError),
    IntoStringError(IntoStringError),
}

impl From<IntoStringError> for EmacsModuleError {
    fn from(error: IntoStringError) -> EmacsModuleError {
        EmacsModuleError::IntoStringError(error)
    }
}

impl From<NulError> for EmacsModuleError {
    fn from(error: NulError) -> EmacsModuleError {
        EmacsModuleError::NulError(error)
    }
}

pub type EmacsInt = ::remacs_sys::EmacsInt;
pub type FuncallExit = ::remacs_sys::emacs_funcall_exit;
#[allow(non_camel_case_types)]
pub type intmax_t = ::libc::intmax_t;

struct UserData {
    func: Box<FnMut(&mut EmacsEnv, &mut Vec<EmacsValue>) -> EmacsValue>,
}

macro_rules! call {
    ($self: ident, $func: ident) => (call!($self, $func,));
    ($self: ident, $func: ident, $($arg:expr),*) => {{
        let fnptr = $self.$func.unwrap();
        unsafe { (fnptr)($self.0, $($arg),*) }
    }}
}

impl EmacsValue {
    pub fn to_raw(&self) -> emacs_value {
        self.0
    }
}

impl Deref for EmacsEnv {
    type Target = emacs_env;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl DerefMut for EmacsEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

impl Deref for EmacsRuntime {
    type Target = emacs_runtime;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl DerefMut for EmacsRuntime {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

impl EmacsRuntime {
    pub fn get_env(&self) -> EmacsEnv {
        EmacsEnv(call!(self, get_environment))
    }
}

#[no_mangle]
pub extern "C" fn springboard(
    env: *mut emacs_env,
    nargs: libc::ptrdiff_t,
    args: *mut emacs_value,
    data: *mut libc::c_void,
) -> emacs_value {
    let mut user_data = unsafe { Box::from_raw(data as *mut UserData) };
    let mut renv = EmacsEnv(env);
    let slice = unsafe { slice::from_raw_parts_mut(args, nargs as usize) };
    let mut arr: Vec<EmacsValue> = slice.iter_mut().map(|x| EmacsValue(*x)).collect();
    let result = {
        let func = &mut user_data.func;
        func(&mut renv, &mut arr)
    };

    mem::forget(user_data);
    result.to_raw()
}

impl EmacsEnv {
    pub fn make_function<T: 'static>(
        &mut self,
        min_args: isize,
        max_args: isize,
        func: T,
        docstr: &str,
    ) -> Result<EmacsValue, EmacsModuleError>
    where
        T: FnMut(&mut EmacsEnv, &mut Vec<EmacsValue>) -> EmacsValue + Sized,
    {
        let cstring = CString::new(docstr)?;
        let boxed = Box::new(UserData {
            func: Box::new(func),
        });

        let result = call!(
            self,
            make_function,
            min_args,
            max_args,
            Some(springboard),
            cstring.as_ptr() as *mut libc::c_char,
            Box::into_raw(boxed) as *mut libc::c_void
        );

        Ok(EmacsValue(result))
    }

    pub fn intern(&mut self, string: &str) -> Result<EmacsValue, EmacsModuleError> {
        let cstring = CString::new(string)?;
        Ok(EmacsValue(
            call!(self, intern, cstring.as_ptr() as *mut libc::c_char),
        ))
    }

    pub fn provide(&mut self, feature: &str) -> Result<EmacsValue, EmacsModuleError> {
        let qfeat = self.intern(feature)?;
        let qprovide = self.intern("provide")?;
        let mut args = vec![qfeat];
        Ok(self.funcall(qprovide, &mut args))
    }

    pub fn funcall(&mut self, fun: EmacsValue, args: &mut Vec<EmacsValue>) -> EmacsValue {
        let len = args.len() as libc::ptrdiff_t;
        let mut raw_array: Vec<emacs_value> = args.iter().map(EmacsValue::to_raw).collect();
        EmacsValue(call!(
            self,
            funcall,
            fun.to_raw(),
            len,
            raw_array.as_mut_ptr()
        ))
    }

    pub fn bind(&mut self, name: &str, fun: EmacsValue) -> Result<EmacsValue, EmacsModuleError> {
        let qset = self.intern("fset")?;
        let qsym = self.intern(name)?;

        let mut args = vec![qsym, fun];
        Ok(self.funcall(qset, &mut args))
    }

    pub fn make_integer(&mut self, num: EmacsInt) -> EmacsValue {
        EmacsValue(call!(self, make_integer, num))
    }

    pub fn make_global_ref(&mut self, any_reference: EmacsValue) -> EmacsValue {
        EmacsValue(call!(self, make_global_ref, any_reference.to_raw()))
    }

    pub fn free_global_ref(&mut self, global_reference: EmacsValue) {
        call!(self, free_global_ref, global_reference.to_raw());
    }

    pub fn non_local_exit_check(&mut self) -> FuncallExit {
        call!(self, non_local_exit_check)
    }

    pub fn non_local_exit_clear(&mut self) {
        call!(self, non_local_exit_clear);
    }

    pub fn non_local_exit_get(&mut self) -> (FuncallExit, EmacsValue, EmacsValue) {
        let mut non_local_exit_symbol_out = 0 as emacs_value;
        let mut non_local_exit_data_out = 0 as emacs_value;
        let result = call!(
            self,
            non_local_exit_get,
            &mut non_local_exit_symbol_out,
            &mut non_local_exit_data_out
        );

        (
            result,
            EmacsValue(non_local_exit_symbol_out),
            EmacsValue(non_local_exit_data_out),
        )
    }

    pub fn non_local_exit_signal(
        &mut self,
        non_local_exit_symbol: EmacsValue,
        non_local_exit_data: EmacsValue,
    ) {
        call!(
            self,
            non_local_exit_signal,
            non_local_exit_symbol.to_raw(),
            non_local_exit_data.to_raw()
        );
    }

    pub fn non_local_exit_throw(&mut self, tag: EmacsValue, value: EmacsValue) {
        call!(self, non_local_exit_throw, tag.to_raw(), value.to_raw());
    }

    pub fn type_of(&self, value: EmacsValue) -> EmacsValue {
        EmacsValue(call!(self, type_of, value.to_raw()))
    }

    pub fn is_not_nil(&self, value: EmacsValue) -> bool {
        call!(self, is_not_nil, value.to_raw())
    }

    pub fn eq(&self, a: EmacsValue, b: EmacsValue) -> bool {
        call!(self, eq, a.to_raw(), b.to_raw())
    }

    pub fn extract_integer(&self, value: EmacsValue) -> intmax_t {
        call!(self, extract_integer, value.to_raw())
    }

    pub fn extract_float(&self, value: EmacsValue) -> f64 {
        call!(self, extract_float, value.to_raw())
    }

    pub fn make_float(&mut self, value: f64) -> EmacsValue {
        EmacsValue(call!(self, make_float, value))
    }

    pub fn copy_string_contents(&self, value: EmacsValue) -> Result<String, EmacsModuleError> {
        let mut required_size = 0;
        let result = call!(
            self,
            copy_string_contents,
            value.to_raw(),
            ptr::null_mut(),
            &mut required_size
        );

        if !result {
            return Err(EmacsModuleError::InvalidValue);
        }

        let mut buffer: Vec<u8> = Vec::with_capacity(required_size as usize);
        let second_result = call!(
            self,
            copy_string_contents,
            value.to_raw(),
            buffer.as_mut_ptr() as *mut libc::c_char,
            &mut required_size
        );

        if !second_result {
            return Err(EmacsModuleError::InvalidValue);
        }

        // The buffer has been filled by the second 'copy_string_contents',
        // but we need to update the vector's internal len to match. We don't need
        // the NULL terminator, otherwise CString::new will panic.
        unsafe { buffer.set_len((required_size - 1) as usize) };
        let string = CString::new(buffer)?.into_string()?;
        Ok(string)
    }

    /// Copy the content of the Lisp string VALUE to BUFFER as an utf8
    /// null-terminated string.
    /// SIZE must point to the total size of the buffer.  If BUFFER is
    /// NULL or if SIZE is not big enough, write the required buffer size
    /// to SIZE and return true.
    /// Note that SIZE must include the last null byte (e.g. "abc" needs
    /// a buffer of size 4).
    /// Return true if the string was successfully copied.
    #[allow(unused_unsafe)]
    pub unsafe fn copy_string_contents_unchecked(
        &self,
        value: EmacsValue,
        buffer: *mut libc::c_char,
        size: *mut isize,
    ) -> bool {
        call!(self, copy_string_contents, value.to_raw(), buffer, size)
    }

    pub fn make_string(&mut self, string: &str) -> Result<EmacsValue, EmacsModuleError> {
        let cstring = CString::new(string)?;
        let len = cstring.to_bytes().len() as isize;
        let ptr = cstring.as_ptr() as *mut libc::c_char;
        Ok(EmacsValue(call!(self, make_string, ptr, len)))
    }

    pub fn vec_get(&self, vec: EmacsValue, i: isize) -> EmacsValue {
        EmacsValue(call!(self, vec_get, vec.to_raw(), i))
    }

    pub fn vec_set(&mut self, vec: EmacsValue, i: isize, val: EmacsValue) {
        call!(self, vec_set, vec.to_raw(), i, val.to_raw())
    }

    pub fn vec_size(&self, vec: EmacsValue) -> isize {
        call!(self, vec_size, vec.to_raw())
    }

    pub fn make_user_ptr(
        &mut self,
        fin: extern "C" fn(*mut libc::c_void),
        ptr: *mut libc::c_void,
    ) -> EmacsValue {
        EmacsValue(call!(self, make_user_ptr, Some(fin), ptr))
    }

    pub fn get_user_ptr(&mut self, uptr: EmacsValue) -> *mut libc::c_void {
        call!(self, get_user_ptr, uptr.to_raw())
    }

    pub fn set_user_ptr(&mut self, uptr: EmacsValue, data: *mut libc::c_void) {
        call!(self, set_user_ptr, uptr.to_raw(), data);
    }

    pub fn set_user_finalizer(&mut self, uptr: EmacsValue, fin: extern "C" fn(*mut libc::c_void)) {
        call!(self, set_user_finalizer, uptr.to_raw(), Some(fin));
    }
}

extern "C" {
    fn module_init(runtime: EmacsRuntime) -> i32;
}

#[no_mangle]
pub extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> libc::c_int {
    let runtime = EmacsRuntime(ert);
    unsafe { module_init(runtime) as libc::c_int }
}

#[allow(non_upper_case_globals)]
#[no_mangle]
pub static plugin_is_GPL_compatible: i32 = 0;
