use libc::ptrdiff_t;
use num;

use crate::remacs_sys::EmacsInt;

#[no_mangle]
pub extern "C" fn clip_to_bounds(lower: ptrdiff_t, num: EmacsInt, upper: ptrdiff_t) -> EmacsInt {
    num::clamp(num as ptrdiff_t, lower, upper) as EmacsInt
}
