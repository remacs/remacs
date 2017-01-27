extern crate tempfile;
extern crate libc;

use std::ffi::{CStr, CString};
use std::mem;
use std::io::Error;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

#[cfg(windows)]
use std::os::windows::AsRawHandle;

#[no_mangle]
pub extern "C" fn rust_make_temp(template: *mut libc::c_char)
                           -> libc::c_int {
    let cstr = unsafe {
        let result = CStr::from_ptr(template).to_str().unwrap();
        assert_eq!(&result[result.len() - 6..result.len()], "XXXXXX");
        &result[0..result.len() - 6]
    };

    match make_temporary_file(cstr) {
        Ok(tempfile) => {
            let dest = tempfile.path().to_str()
                .map(|path| CString::new(path).unwrap())
                .unwrap();

            let raw_fd = get_fd_from_file(&tempfile);
            unsafe {
                libc::strcpy(template, dest.as_ptr());
                mem::forget(tempfile);
            }
            
            raw_fd
        }

        Err(_) => -1
    }
}

#[cfg(unix)]
fn get_fd_from_file(file: &tempfile::NamedTempFile) -> libc::c_int {
    file.as_raw_fd()
}

#[cfg(windows)]
fn get_fd_from_file(file: &tempfile::NamedTempFile) -> libc::c_int {
    file.as_raw_handle()
}

fn make_temporary_file(template: &str) -> Result<tempfile::NamedTempFile, Error> {
    tempfile::NamedTempFileOptions::new()
        .prefix(template)
        .rand_bytes(6)
        .create()
}
