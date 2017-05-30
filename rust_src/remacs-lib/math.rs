use libc;

// Macro used to generate a c function that wraps functionality provided by the Rust stdlib
// for u32 and other unsigned types.
macro_rules! gen_unsigned_wrapper_fn {
    ($fn_name: ident, $invoke: ident, $type: ty) => {
        #[no_mangle]
        pub extern "C" fn $fn_name(bar: $type) -> libc::c_int {
            bar.$invoke() as libc::c_int
        }
    }
}

gen_unsigned_wrapper_fn!(rust_count_trailing_zeros, trailing_zeros, libc::c_uint);
gen_unsigned_wrapper_fn!(rust_count_trailing_zeros_l, trailing_zeros, libc::c_ulong);
gen_unsigned_wrapper_fn!(rust_count_trailing_zeros_ll,
                         trailing_zeros,
                         libc::c_ulonglong);

gen_unsigned_wrapper_fn!(rust_count_one_bits, count_ones, libc::c_uint);
gen_unsigned_wrapper_fn!(rust_count_one_bits_l, count_ones, libc::c_ulong);
gen_unsigned_wrapper_fn!(rust_count_one_bits_ll, count_ones, libc::c_ulonglong);
