use std::fs::File;
use std::io::Error;

use std::os::unix::io::IntoRawFd;

extern crate tempfile;
extern crate libc;

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn mkrstemp(template: *mut libc::c_char,
                          flags: libc::c_int) -> libc::c_int {

    match make_temporary_file() {
        Ok(tempFile) => tempFile.into_raw_fd(),
        Err(err) => -1,
    }
}

pub fn make_temporary_file() -> Result<File, Error> {
    tempfile::tempfile()
}
