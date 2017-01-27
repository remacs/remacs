use std::fs::File;
use std::io::{Write, Read, Seek, SeekFrom, Error};

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

#[test]
fn test_make_temporary_file() {
    let mut file = make_temporary_file().unwrap();
    file.write_all(b"Attempting to write temporary data").unwrap();

    file.seek(SeekFrom::Start(0)).unwrap();
    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();
    assert_eq!(string
               ,"Attempting to write temporary data");
}
