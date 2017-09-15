//! Miscellaneous utility functions

use remacs_sys::EmacsInt;

#[no_mangle]
pub extern "C" fn clip_to_bounds(lower: isize, num: EmacsInt, upper: isize) -> isize {
    let num = num as isize;
    if num < lower {
        lower
    } else if num <= upper {
        num
    } else {
        upper
    }
}
