//! Miscellaneous utility functions

use remacs_sys::EmacsInt;

use threads::ThreadState;

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

/// Get boundaries of the current buffer.
/// If BYTE is true, return byte positions.
/// If RESTRICTED is true, limit the bounds by the buffer's visible part.
pub fn current_buffer_bounds(byte: bool, restricted: bool) -> (isize, isize) {
    let cur_buf = ThreadState::current_buffer();

    let beg = if restricted {
        if byte {
            cur_buf.begv_byte
        } else {
            cur_buf.begv
        }
    } else {
        if byte {
            cur_buf.beg_byte()
        } else {
            cur_buf.beg()
        }
    };

    let end = if restricted {
        if byte {
            cur_buf.zv_byte
        } else {
            cur_buf.zv
        }
    } else {
        if byte {
            cur_buf.z_byte()
        } else {
            cur_buf.z()
        }
    };

    (beg, end)
}
