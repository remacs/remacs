#![allow(non_camel_case_types)]

pub type EMACS_INT = usize;
pub type Lisp_Object = EMACS_INT;

pub mod lisp {
    use super::*;

    pub struct LispObject(Lisp_Object);

    impl LispObject {
        pub fn from_raw(raw: Lisp_Object) -> LispObject {
            LispObject(raw)
        }

        pub fn is_symbol(&self) -> bool {
            true
        }

        pub fn constant_t() -> LispObject {
            LispObject(1)
        }

        pub fn constant_nil() -> LispObject {
            LispObject(0)
        }

        pub fn to_raw(&self) -> Lisp_Object {
            0
        }
    }
}
