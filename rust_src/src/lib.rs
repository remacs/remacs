#[no_mangle]
pub unsafe extern "C" fn rust_hello() {
    println!("hello from rust!");
}
