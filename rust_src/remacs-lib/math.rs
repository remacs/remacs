use libc;

#[no_mangle]
pub extern "C" fn rust_count_trailing_zero_bits(val: libc::size_t) -> libc::c_int {
    val.trailing_zeros() as libc::c_int
}

#[no_mangle]
pub extern "C" fn rust_count_one_bits(val: libc::size_t) -> libc::c_int {
    val.count_ones() as libc::c_int
}
