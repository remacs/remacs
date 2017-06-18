use libc::{c_int, size_t};

#[no_mangle]
pub extern "C" fn rust_count_trailing_zero_bits(val: size_t) -> c_int {
    val.trailing_zeros() as c_int
}

#[no_mangle]
pub extern "C" fn rust_count_one_bits(val: size_t) -> c_int {
    val.count_ones() as c_int
}
