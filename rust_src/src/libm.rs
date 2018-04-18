use libc::c_int;
use std::mem;

mod sys {
    use libc::{c_double, c_int};

    #[link_name = "m"]
    extern "C" {
        pub fn frexp(n: c_double, value: &mut c_int) -> c_double;
        pub fn ldexp(x: c_double, n: c_int) -> c_double;
        pub fn rint(x: c_double) -> c_double;
    }
}

/// Return the sign bit of the float.
pub fn signbit(x: f64) -> bool {
    let bits: u64 = unsafe { mem::transmute(x) };
    bits >> 63 != 0
}

/// Split the number `x` into a normalized fraction and an exponent.
pub fn frexp(x: f64) -> (f64, c_int) {
    let mut n: c_int = 0;
    let f = unsafe { sys::frexp(x, &mut n) };
    (f, n)
}

/// Return the result of multiplying the floating-point number `x` by 2
/// raised to the power exp.
pub fn ldexp(x: f64, exp: c_int) -> f64 {
    unsafe { sys::ldexp(x, exp) }
}

/// Round `x` to an integer value in floating-point format.
pub fn rint(x: f64) -> f64 {
    unsafe { sys::rint(x) }
}
