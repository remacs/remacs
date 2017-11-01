/// This module is loaded only in #[cfg(test)].
/// It contains the definitions of C functions to be mocked in our tests
/// Due to the fact that cfg(test) does not cascade to dependent crates,
/// we need to duplicate the extern "C" block found in remacs-sys,
/// so that our the #[mock] macro can generate the code
/// we need for mocking in our tests.

/// Adding a function to this block is harmless.
/// This module is only for testing, and you should add all
/// definitions to remacs-sys first and foremost.
use libc::c_double;

use mock_derive::mock;

use remacs_sys::Lisp_Object;

#[mock]
extern "C" {
    pub fn make_float(float_value: c_double) -> Lisp_Object;
}
