use lisp::{LispObject, FLOATP, XFLOAT_DATA, XINT};

extern "C" {
    fn make_float(float_value: f64) -> LispObject;
}

/// Calculate the modulus of two elisp floats.
pub fn fmod_float(x: LispObject, y: LispObject) -> LispObject {
    let mut f1: f64 = if FLOATP(x) {
        XFLOAT_DATA(x)
    } else {
        XINT(x) as f64
    };
    let f2: f64 = if FLOATP(y) {
        XFLOAT_DATA(y)
    } else {
        XINT(y) as f64
    };

    f1 %= f2;
        
    // Ensure that the remainder has the correct sign.
    if f2 < 0.0 && f1 > 0.0 || f2 > 0.0 && f1 < 0.0 {
        f1 += f2
    }

    unsafe {
        make_float(f1)
    }
}
