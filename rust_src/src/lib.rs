#![feature(libc)]

// TODO: move to libc from crates.io
extern crate libc;

// Use Emacs naming conventions.
#[allow(non_upper_case_globals)]

// TODO: typedef EMACS_INT to long int
//
// note this is dependent on platform and compiler flags passed when
// compiling emacs.

// First, we need a reference to Qt, the t symbol.
static Qt: i64 = 1;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
