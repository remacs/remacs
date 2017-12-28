//! Functions operating on numbers.

use rand::{Rng, SeedableRng, StdRng};
use std::sync::Mutex;

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, INTMASK};

use lisp::LispObject;
use lisp::defsubr;

lazy_static! {
    static ref RNG: Mutex<StdRng> = Mutex::new(StdRng::new().unwrap());
}

/// Return t if OBJECT is a floating point number.
#[lisp_fn]
pub fn floatp(object: LispObject) -> bool {
    object.is_float()
}

/// Return t if OBJECT is an integer.
#[lisp_fn]
pub fn integerp(object: LispObject) -> bool {
    object.is_integer()
}

/// Return t if OBJECT is an integer or a marker (editor pointer).
#[lisp_fn]
pub fn integer_or_marker_p(object: LispObject) -> bool {
    object.is_marker() || object.is_integer()
}

/// Return t if OBJECT is a non-negative integer.
#[lisp_fn]
pub fn natnump(object: LispObject) -> bool {
    object.is_natnum()
}

/// Return t if OBJECT is a number (floating point or integer).
#[lisp_fn]
pub fn numberp(object: LispObject) -> bool {
    object.is_number()
}

/// Return t if OBJECT is a number or a marker (editor pointer).
#[lisp_fn]
pub fn number_or_marker_p(object: LispObject) -> bool {
    object.is_number() || object.is_marker()
}

/// Return a pseudo-random number.
/// All integers representable in Lisp, i.e. between `most-negative-fixnum'
/// and `most-positive-fixnum', inclusive, are equally likely.
///
/// With positive integer LIMIT, return random number in interval [0,LIMIT).
/// With argument t, set the random number seed from the system's entropy
/// pool if available, otherwise from less-random volatile data such as the time.
/// With a string argument, set the seed based on the string's contents.
/// Other values of LIMIT are ignored.
///
/// See Info node `(elisp)Random Numbers' for more details.
// NOTE(db48x): does not return an EmacsInt, because it relies on the
// truncating behavior of from_fixnum_truncated.
#[lisp_fn(min = "0")]
pub fn random(limit: LispObject) -> LispObject {
    let mut rng = RNG.lock().unwrap();
    if limit.is_t() {
        *rng = StdRng::new().unwrap();
    } else if let Some(s) = limit.as_string() {
        let values: Vec<usize> = s.as_slice().iter().map(|&x| x as usize).collect();
        rng.reseed(&values);
    }

    if let Some(limit) = limit.as_fixnum() {
        // Return the remainder, except reject the rare case where
        // get_random returns a number so close to INTMASK that the
        // remainder isn't random.
        loop {
            let val: EmacsInt = rng.gen();
            let remainder = val.abs() % limit;
            if val - remainder <= INTMASK - limit + 1 {
                return LispObject::from_fixnum(remainder);
            }
        }
    } else {
        LispObject::from_fixnum_truncated(rng.gen())
    }
}

include!(concat!(env!("OUT_DIR"), "/numbers_exports.rs"));
