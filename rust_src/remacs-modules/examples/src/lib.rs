extern crate remacs_modules;

use remacs_modules::EmacsRuntime;

/// The entry point for your module.
/// For now this need to be #[no_mangle] and extern "C"
/// Remacs will call this when your module is required.
/// As a note, Remacs will look for <modname>.so. Cargo likes to name
/// the produced libs something remacs isn't expecting, so you can either
/// a) rename the result to <mymod>.so or <mymod>.dll on windows.
/// b) call (module-load "<cargo name of mymod>")
#[no_mangle]
pub extern "C" fn module_init(runtime: EmacsRuntime) -> i32 {
    let mut env = runtime.get_env();
    let fun = env.make_function(
        0, // min args
        0, // max args
        // Your callback
        |env, _args| env.make_integer(43),
        "make a number", // docstring
    );

    env.bind("mymod-func", fun); // Bind 'mymod-func' to our created function
    env.provide("mymod"); // provide our library so we can 'require' it

    0 // Success!
}
