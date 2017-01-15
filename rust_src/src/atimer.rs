use std::os::raw::c_void;
use std::ptr::null_mut;
use std::mem::uninitialized;

use libc::sigset_t;
use libc::c_int;
use libc::sigemptyset;
use libc::sigaddset;
use libc::pthread_sigmask;
use libc::timespec;
use libc::{SIGALRM, SIGINT, SIG_BLOCK, SIG_SETMASK};

extern "C" {
    static mut free_atimers: *mut Atimer;
    static mut stopped_atimers: *mut Atimer;
    static mut atimers: *mut Atimer;

    static mut pending_signals: i32;
}

// Types of timers.
// FIXME How large this enum is in C?
#[repr(u8)]
#[derive(PartialEq, Eq)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
pub enum AtimerType {
    // Timer is ripe at some absolute time.
    ATIMER_ABSOLUTE,

    // Timer is ripe at now plus an offset.
    ATIMER_RELATIVE,

    // Timer runs continuously.
    ATIMER_CONTINUOUS,
}

// Structure describing an asynchronous timer.
#[repr(C)]
pub struct Atimer {
    // The type of this timer.
    timer_type: AtimerType,

    // Time when this timer is ripe.
    expiration: timespec,

    // Interval of this timer.
    interval: timespec,

    // Function to call when timer is ripe.  Interrupt input is
    // guaranteed to not be blocked when this function is called.
    // XXX Temporarily placeholder.
    func: *const c_void,

    // Additional user-specified data to pass to FN.
    client_data: *const c_void,

    // Next in list of active or free atimers.
    next: *mut Atimer,
}

// This is the Rust port of timespec_cmp from timespec.h
fn timespec_cmp (a: timespec, b: timespec) -> c_int {
    match (a.tv_sec < b.tv_sec, a.tv_sec > b.tv_sec) {
        (true, _) => -1,
        (false, true) => 1,
        (false, false) => {
            (a.tv_nsec - b.tv_nsec) as c_int
        }
    }
}

// Block/unblock SIGALRM.
#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn block_atimers(oldset: *mut sigset_t) {
    let mut blocked: sigset_t = unsafe { uninitialized() };

    unsafe {
        sigemptyset(&mut blocked);
        sigaddset(&mut blocked, SIGALRM);
        sigaddset(&mut blocked, SIGINT);
        pthread_sigmask(SIG_BLOCK, &mut blocked, oldset);
    }
}

#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn unblock_atimers(oldset: *const sigset_t) {
    unsafe {
        pthread_sigmask(SIG_SETMASK, oldset, null_mut());
    }
}

// Insert timer T into the list of active atimers `atimers', keeping
// the list sorted by expiration time. T must not be in this list
// already.
#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn schedule_atimer(t: *mut Atimer) {
    let mut a = unsafe { atimers };
    let mut prev = null_mut();

    unsafe {
        // Look for the first atimer that is ripe after T.
        while !a.is_null() && (timespec_cmp((*a).expiration, (*t).expiration) < 0) {
            prev = a;
            a = (*a).next;
        }
    }

    // Insert T in front of the atimer found, if any.
    if !prev.is_null() {
        unsafe {
            (*prev).next = t;
        }
    } else {
        unsafe {
            atimers = t;
        }
    }

    unsafe {
        (*t).next = a;
    }
}

// Signal handler for SIGALRM.  SIGNO is the signal number, i.e. SIGALRM.
#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn handle_alarm_signal (_sig: c_int) {
    unsafe {
        pending_signals = 1;
    }
}

// Append two lists of atimers LIST_1 and LIST_2 and return the result list.
fn append_atimer_lists(list_1: *mut Atimer, list_2: *mut Atimer) -> *mut Atimer {
    match (list_1.is_null(), list_2.is_null()) {
        (true, _) => list_2,
        (false, true) => list_1,
        _ => {
            let mut p = list_1;

            unsafe {
                while !(*p).next.is_null() {
                    p = (*p).next;
                }

                (*p).next = list_2;
            }

            list_1
        }
    }
}

// Cancel and free atimer TIMER.
#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn cancel_atimer(timer: *const Atimer) {
    let mut oldset: sigset_t = unsafe { uninitialized() };
    block_atimers(&mut oldset);

    let timer_list: Vec<*mut *mut Atimer>;
    unsafe {
        timer_list = vec![&mut stopped_atimers, &mut atimers];
    }

    for list in timer_list {
        let mut t: *mut Atimer = unsafe { *list };
        let mut prev: *mut Atimer = null_mut();

        // See if TIMER is active or stopped.
        while !t.is_null() && (t as *const Atimer != timer) {
            prev = t;
            unsafe {
                t = (*t).next;
            }
        }

        // If it is, take it off its list, and put in on the free-list.
        // We don't bother to arrange for setting a different alarm time,
        // since a too early one doesn't hurt.
        if !t.is_null() {
            if !prev.is_null() {
                unsafe {
                    (*prev).next = (*t).next;
                }
            } else {
                unsafe {
                    *list = (*t).next;
                }
            }

            unsafe {
                (*t).next = free_atimers;
                free_atimers = t;
            }

            break;
        }
    }

    unblock_atimers(&oldset);
}


// Stop all timers except timer T.  T null means stop all timers.
#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn stop_other_atimers(mut t: *mut Atimer) {
    let mut oldset: sigset_t = unsafe { uninitialized() };
    block_atimers(&mut oldset);

    if !t.is_null() {
        let mut p = unsafe { atimers };
        let mut prev = null_mut::<Atimer>();

        while !p.is_null() && (p as *const Atimer != t) {
            prev = p;

            unsafe {
                p = (*p).next;
            }
        }

        if p as *const Atimer == t {
            match prev.is_null() {
                true => unsafe { atimers = (*t).next; },
                false => unsafe { (*prev).next = (*t).next; },
            }

            unsafe {
                (*t).next = null_mut();
            }
        } else {
            // T is not active.  Let's handle this like T == 0.
            t = null_mut();
        }
    }

    unsafe {
        stopped_atimers = append_atimer_lists (atimers, stopped_atimers);
        atimers = t;
    }

    unblock_atimers (&oldset);
}

// Run all timers again, if some have been stopped with a call to stop_other_atimers.
#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn run_all_atimers() {
    let stopped_atimers_is_not_null = unsafe { !stopped_atimers.is_null() };

    if stopped_atimers_is_not_null {
        let mut t = unsafe { atimers };
        let mut oldset: sigset_t = unsafe { uninitialized() };

        block_atimers (&mut oldset);

        unsafe {
            atimers = stopped_atimers;
            stopped_atimers = null_mut();
        }

        while !t.is_null() {
            let next;

            unsafe {
                next = (*t).next;
                schedule_atimer(t);
            }

            t = next;
        }

        unblock_atimers (&oldset);
    }
}
