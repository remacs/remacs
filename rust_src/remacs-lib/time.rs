//! Time support

use crate::time_crate::now;
use libc::c_long;
use libc::timespec as c_timespec;

/// Return current system time.
#[no_mangle]
pub extern "C" fn current_timespec() -> c_timespec {
    let ts = now().to_timespec();
    c_timespec {
        tv_sec: ts.sec,
        tv_nsec: c_long::from(ts.nsec),
    }
}
