extern crate libc;
extern crate rand;

use std::ffi::{CStr, CString};
use std::io;
#[cfg(unix)]
use libc::{O_CLOEXEC, O_EXCL, O_RDWR, O_CREAT, open};

#[cfg(test)]
use std::{env, ptr};

use self::rand::Rng;

const NUM_RETRIES: usize = 50;

//@TODO The third parameter of this will be an error code
#[no_mangle]
pub extern "C" fn rust_make_temp(template: *mut libc::c_char,
                                 flags: libc::c_int,
                                 _: *mut libc::c_int)
                                 -> libc::c_int {

    let rust_template = unsafe { CStr::from_ptr(template) };
    let mut owned_template_copy = rust_template.to_string_lossy().into_owned();
    let mut file_handle = -1;
    for _ in 0..NUM_RETRIES {
        generate_temporary_filename(&mut owned_template_copy);
        let attempt_name = CString::new(owned_template_copy.clone()).unwrap();
        file_handle = match open_temporary_file(&attempt_name, flags) {
            Ok(file) => file,
            Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(_) => -1
        };

        if file_handle != -1 {
            unsafe { libc::strcpy(template, attempt_name.as_ptr()); }           
            break;
        }
    };

    file_handle
}

fn generate_temporary_filename(name: &mut String) {
    let len = name.len();
    assert!(len >= 6);
    unsafe {
        let mut name_vec = &mut name.as_mut_vec();
        let mut bytes = &mut name_vec[len - 6..len];
        rand::thread_rng().fill_bytes(bytes);
        for byte in bytes.iter_mut() {
            *byte = match *byte % 62 {
                v @ 0...9 => (v + b'0' as u8),
                v @ 10...35 => (v - 10 + b'a' as u8),
                v @ 36...61 => (v - 36 + b'A' as u8),
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(unix)]
fn open_temporary_file(name: &CString, flags: libc::c_int) -> io::Result<libc::c_int> {
    unsafe {
        match open(name.as_ptr(),
             O_CLOEXEC | O_EXCL | O_RDWR | O_CREAT | flags,
                   0o600) {
            -1 => Err(io::Error::last_os_error()),
            fd => Ok(fd),
        }
    }
}

// @TODO Need to implement a windows version of open_temporary_file
// it will use sys_open as an extern "C" function

#[test]
#[should_panic]
fn test_generate_bad_temporary_filename() {
    let mut bad_name = String::from("1234");
    generate_temporary_filename(&mut bad_name);
}

#[test]
fn test_generate_temporary_filename() {
    let mut name = String::from(".emacs-XXXXXX");
    let name_copy = name.clone();
    generate_temporary_filename(&mut name);
    assert!(name != name_copy, "Temporary filename should always mutate the name string");
}

#[test]
fn test_generate_temporary_filename_change() {
    let mut name = String::from(".emacs-XXXXXX");
    generate_temporary_filename(&mut name);
    assert!(!name.contains("-XXXXXX"));
}

#[test]
fn test_rust_make_temp() {
    let mut tmpdir = env::temp_dir();
    tmpdir.push(".emacs-XXXXXX");
    let fullpath = tmpdir.to_string_lossy().into_owned();
    let name = CString::new(fullpath).unwrap();
    let name_copy = name.clone();
    let raw_ptr = name.into_raw();
    let file_handle = rust_make_temp(raw_ptr, 0, ptr::null_mut());
    assert!(file_handle != -1);
    let new_name = unsafe { CString::from_raw(raw_ptr) };
    assert!(new_name != name_copy);
}
