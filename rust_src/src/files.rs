use std::fs::File;
use std::io::Error;

use std::os::unix::io::IntoRawFd;

extern crate tempfile;
extern crate libc;

#[no_mangle]
pub extern "C" fn mkrstemp(_: *mut libc::c_char,
                           _: libc::c_int)
                           -> libc::c_int {

    match make_temporary_file() {
        Ok(tempFile) => tempFile.into_raw_fd(),
        Err(_) => -1,
    }
}

pub fn make_temporary_file() -> Result<File, Error> {
    tempfile::tempfile()
}
