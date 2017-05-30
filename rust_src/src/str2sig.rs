//! Convert between signal names and numbers.

use libc::{self, c_char, c_int};
use std::ffi::CStr;
use std::str::FromStr;

#[cfg(not(unix))]
const numname: [(&'static str, c_int); 0] = [];

#[cfg(all(unix, not(target_os = "macos")))]
const numname: [(&'static str, c_int); 30] = [("HUP", libc::SIGHUP),
                                              ("INT", libc::SIGINT),
                                              ("QUIT", libc::SIGQUIT),
                                              ("ILL", libc::SIGILL),
                                              ("TRAP", libc::SIGTRAP),
                                              ("ABRT", libc::SIGABRT),
                                              ("IOT", libc::SIGIOT),
                                              ("BUS", libc::SIGBUS),
                                              ("FPE", libc::SIGFPE),
                                              ("KILL", libc::SIGKILL),
                                              ("USR1", libc::SIGUSR1),
                                              ("USR2", libc::SIGUSR2),
                                              ("SEGV", libc::SIGSEGV),
                                              ("PIPE", libc::SIGPIPE),
                                              ("ALRM", libc::SIGALRM),
                                              ("TERM", libc::SIGTERM),
                                              ("STKFLT", libc::SIGSTKFLT),
                                              ("CHLD", libc::SIGCHLD),
                                              ("CONT", libc::SIGCONT),
                                              ("STOP", libc::SIGSTOP),
                                              ("TSTP", libc::SIGTSTP),
                                              ("TTIN", libc::SIGTTIN),
                                              ("TTOU", libc::SIGTTOU),
                                              ("URG", libc::SIGURG),
                                              ("XCPU", libc::SIGXCPU),
                                              ("PROF", libc::SIGPROF),
                                              ("WINCH", libc::SIGWINCH),
                                              ("POLL", libc::SIGPOLL),
                                              ("PWR", libc::SIGPWR),
                                              ("SYS", libc::SIGSYS)];

#[cfg(target_os = "macos")]
const numname: [(&'static str, c_int); 27] = [("HUP", libc::SIGHUP),
                                              ("INT", libc::SIGINT),
                                              ("QUIT", libc::SIGQUIT),
                                              ("ILL", libc::SIGILL),
                                              ("TRAP", libc::SIGTRAP),
                                              ("ABRT", libc::SIGABRT),
                                              ("IOT", libc::SIGIOT),
                                              ("BUS", libc::SIGBUS),
                                              ("FPE", libc::SIGFPE),
                                              ("KILL", libc::SIGKILL),
                                              ("USR1", libc::SIGUSR1),
                                              ("USR2", libc::SIGUSR2),
                                              ("SEGV", libc::SIGSEGV),
                                              ("PIPE", libc::SIGPIPE),
                                              ("ALRM", libc::SIGALRM),
                                              ("TERM", libc::SIGTERM),
                                              ("CHLD", libc::SIGCHLD),
                                              ("CONT", libc::SIGCONT),
                                              ("STOP", libc::SIGSTOP),
                                              ("TSTP", libc::SIGTSTP),
                                              ("TTIN", libc::SIGTTIN),
                                              ("TTOU", libc::SIGTTOU),
                                              ("URG", libc::SIGURG),
                                              ("XCPU", libc::SIGXCPU),
                                              ("PROF", libc::SIGPROF),
                                              ("WINCH", libc::SIGWINCH),
                                              ("SYS", libc::SIGSYS)];

/// Convert the signal name SIGNAME to the signal number
/// *SIGNUM. Return 0 if successful, -1 otherwise.
#[no_mangle]
pub unsafe extern "C" fn str2sig(signame: *const c_char, signum: *mut c_int) -> c_int {
    let s = CStr::from_ptr(signame).to_string_lossy();
    match FromStr::from_str(s.as_ref()) {
        Ok(i) => {
            *signum = i;
            return 0;
        }
        Err(_) => {
            for &(name, num) in numname.iter() {
                if name == s {
                    *signum = num;
                    return 0;
                }
            }
        }
    }
    return -1;
}
