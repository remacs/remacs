//! Call a Lisp function interactively.

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::{EmacsInt, Qminus},
};

#[derive(Copy, Clone, Debug)]
pub enum InteractiveNumericPrefix {
    Object(LispObject),
    Number(EmacsInt),
}

impl InteractiveNumericPrefix {
    pub fn from_number(value: EmacsInt) -> Self {
        Self::Number(value)
    }

    pub fn from_object(raw: LispObject) -> Self {
        Self::Object(raw)
    }

    pub fn is_minus(self) -> bool {
        match self {
            Self::Number(_) => false,
            Self::Object(raw) => raw.eq(Qminus),
        }
    }

    pub fn unwrap(self) -> EmacsInt {
        match self {
            Self::Number(value) => value,
            Self::Object(raw) => {
                if raw.is_nil() {
                    1
                } else if raw.eq(Qminus) {
                    -1
                } else if raw.is_integer() {
                    raw.into()
                } else if let Some(number) = raw.as_cons().and_then(|v| v.car().as_fixnum()) {
                    number
                } else {
                    1
                }
            }
        }
    }
}

impl From<LispObject> for InteractiveNumericPrefix {
    fn from(obj: LispObject) -> Self {
        Self::from_object(obj)
    }
}

impl From<LispObject> for Option<InteractiveNumericPrefix> {
    fn from(obj: LispObject) -> Self {
        if obj.is_nil() {
            None
        } else {
            Some(InteractiveNumericPrefix::from_object(obj))
        }
    }
}

impl From<EmacsInt> for InteractiveNumericPrefix {
    fn from(n: EmacsInt) -> Self {
        Self::from_number(n)
    }
}

/// Return numeric meaning of raw prefix argument RAW.
/// A raw prefix argument is what you get from `(interactive "P")'.
/// Its numeric meaning is what you would get from `(interactive "p")'.
#[lisp_fn]
pub fn prefix_numeric_value(raw: InteractiveNumericPrefix) -> EmacsInt {
    raw.unwrap()
}

include!(concat!(env!("OUT_DIR"), "/interactive_exports.rs"));
