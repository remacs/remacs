extern crate remacs_sys;
extern crate libc;

use remacs_sys::{emacs_runtime, emacs_env, emacs_value};
use std::ops::{Deref, DerefMut};
use std::ffi::CString;
use std::mem;
use std::slice;

#[repr(C)]
pub struct EmacsRuntime(*mut emacs_runtime);

#[repr(C)]
pub struct EmacsEnv(*mut emacs_env);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct EmacsValue(emacs_value);

pub type EmacsInt = ::remacs_sys::EmacsInt;

struct UserData {
    func: Box<FnMut(&mut EmacsEnv, &mut Vec<EmacsValue>, *mut libc::c_void) -> EmacsValue>,
    data: *mut libc::c_void,
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

impl EmacsRuntime {
    pub fn get_env(&self) -> EmacsEnv {
        unsafe {
            let fnptr = (*self.0).get_environment.unwrap();
            EmacsEnv((fnptr)(self.0))
        }
    }
}

#[no_mangle]
pub extern "C" fn springboard(env: *mut emacs_env, nargs: libc::ptrdiff_t, args: *mut emacs_value, data: *mut libc::c_void) -> emacs_value {
    let mut user_data = unsafe { Box::from_raw(data as *mut UserData) };
    let mut renv = EmacsEnv(env);
    let slice = unsafe { slice::from_raw_parts_mut(args, nargs as usize) };
    let mut arr: Vec<EmacsValue> = slice.iter_mut().map(|x| EmacsValue(*x)).collect();
    let result = {
        let data = user_data.data;
        let func = &mut user_data.func;
        func(&mut renv, &mut arr, data)
    };

    mem::forget(user_data);
    result.to_raw()
}

impl EmacsEnv {
    pub fn make_function(&mut self,
                         min_args: isize,
                         max_args: isize,
                         func: Box<FnMut(&mut EmacsEnv, &mut Vec<EmacsValue>, *mut libc::c_void) -> EmacsValue>,
                         docstr: &str,
                         data: *mut libc::c_void) -> EmacsValue {
        unsafe {
            let fnptr = self.make_function.unwrap();
            let cstring = CString::new(docstr).unwrap();
            let boxed = Box::new(UserData {
                func,
                data,
            });
            
            let result = (fnptr)(self.0,
                                 min_args,
                                 max_args,
                                 Some(springboard),
                                 cstring.as_ptr(),
                                 Box::into_raw(boxed) as *mut libc::c_void);
            EmacsValue(result)
        }
    }

    pub fn intern(&mut self, string: &str) -> EmacsValue {
        let cstring = CString::new(string).unwrap();
        let fnptr = self.intern.unwrap();
        EmacsValue(unsafe { (fnptr)(self.0, cstring.as_ptr()) })
    }

    pub fn provide(&mut self, feature: &str) {
        let qfeat = self.intern(feature);
        let qprovide = self.intern("provide");
        let mut args = vec![qfeat];
        self.funcall(qprovide, &mut args);
    }

    pub fn funcall(&mut self, fun: EmacsValue, args: &mut Vec<EmacsValue>) -> EmacsValue {
        let fnptr = self.funcall.unwrap();
        let len = args.len() as libc::ptrdiff_t;
        let mut raw_array: Vec<emacs_value> = args.iter().map(EmacsValue::to_raw).collect();
        EmacsValue(unsafe { (fnptr)(self.0, fun.to_raw(), len, raw_array.as_mut_ptr()) })

    }

    pub fn bind(&mut self, name: &str, fun: EmacsValue) {
        let qset = self.intern("fset");
        let qsym = self.intern(name);

        let mut args = vec![qsym, fun];
        self.funcall(qset, &mut args);
    }

    pub fn make_integer(&mut self, num: EmacsInt) -> EmacsValue {
        let fnptr = self.make_integer.unwrap();
        EmacsValue(unsafe { (fnptr)(self.0, num) })
    }
}

extern "C" {
    fn module_init (runtime: EmacsRuntime) -> i32;
}

#[no_mangle]
pub extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> libc::c_int {
    let runtime = EmacsRuntime(ert);
    unsafe { module_init(runtime) as libc::c_int }
}

#[allow(non_upper_case_globals)]
#[no_mangle]
pub static plugin_is_GPL_compatible: i32 = 0;
