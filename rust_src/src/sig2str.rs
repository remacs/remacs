/// Convert between signal names and numbers.

use libc::{self, c_char, c_int};

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
pub extern "C" fn str2sig(signame: *const c_char, signum: *mut c_int) -> c_int {
    return -1;
}

/// Convert SIGNUM to a signal name in SIGNAME. SIGNAME must point to
/// a buffer of at least 5 bytes. Return 0 if successful, -1
/// otherwise.
#[no_mangle]
pub extern "C" fn sig2str(signum: c_int, signame: *mut c_char) -> c_int {
    return -1;
}
