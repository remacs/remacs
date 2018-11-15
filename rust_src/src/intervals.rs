//! Intervals support

use std::cmp::min;
use std::ptr;

use crate::{
    lisp::{ExternalPtr, LispObject},
    multibyte::LispStringRef,
    remacs_sys::balance_possible_root_interval,
    remacs_sys::{interval, INTERVAL},
};

pub type IntervalRef = ExternalPtr<interval>;

impl IntervalRef {
    // True if this is a default interval, which is the same as being null
    // or having no properties.
    pub fn is_default(self) -> bool {
        self.is_null() || self.plist.is_nil()
    }

    pub fn length(self) -> isize {
        self.total_length - self.right_total_length() - self.left_total_length()
    }

    pub fn get_total_length(self) -> isize {
        // The total size of all text represented by this interval and all its
        // children in the tree.
        // This is zero if the interval is null.
        if self.is_null() {
            0
        } else {
            self.total_length
        }
    }

    // The total size of the left subtree of this interval.
    pub fn left_total_length(self) -> isize {
        IntervalRef::new(self.left).get_total_length()
    }

    // The total size of the right subtree of this interval.
    pub fn right_total_length(self) -> isize {
        IntervalRef::new(self.right).get_total_length()
    }

    pub fn last_pos(self) -> isize {
        self.position + self.length()
    }

    pub fn has_null_parent(self) -> bool {
        self.up_obj() || unsafe { self.up.interval.is_null() }
    }

    pub fn has_null_left_child(self) -> bool {
        self.left.is_null()
    }

    pub fn has_null_right_child(self) -> bool {
        self.right.is_null()
    }

    pub fn am_left_child(mut self) -> bool {
        !self.has_null_parent() && self.parent().left == self.as_mut()
    }

    pub fn am_right_child(mut self) -> bool {
        !self.has_null_parent() && self.parent().right == self.as_mut()
    }

    pub fn parent(self) -> IntervalRef {
        assert!(!(self.is_null() || self.up_obj()));
        IntervalRef::new(unsafe { self.up.interval })
    }

    pub fn has_object(self) -> bool {
        self.up_obj()
    }

    pub fn get_object(self) -> LispObject {
        assert!(self.has_object());
        unsafe { self.up.obj }
    }
}

// Return true if strings S1 and S2 have identical properties.
// Assume they have identical characters.
#[no_mangle]
pub extern "C" fn compare_string_intervals(s1: LispObject, s2: LispObject) -> bool {
    compare_string_intervals_rust(s1.as_string_or_error(), s2.as_string_or_error())
}

pub fn compare_string_intervals_rust(s1: LispStringRef, s2: LispStringRef) -> bool {
    let mut pos = 0;
    let end = s1.len_chars();

    let mut i1 = find_interval_rust(s1.intervals(), 0);
    let mut i2 = find_interval_rust(s2.intervals(), 0);

    while pos < end {
        // Determine how far we can go before we reach the end of I1 or I2.
        let len1 = if i1.is_null() { end } else { i1.last_pos() } - pos;
        let len2 = if i2.is_null() { end } else { i2.last_pos() } - pos;
        let distance = min(len1, len2);

        // If we ever find a mismatch between the strings, they differ.
        if !intervals_equal_rust(i1, i2) {
            return false;
        }

        // Advance POS till the end of the shorter interval,
        // and advance one or both interval pointers for the new position.
        pos += distance;
        if len1 == distance {
            i1 = next_interval_rust(i1);
        }
        if len2 == distance {
            i2 = next_interval_rust(i2);
        }
    }

    true
}

// Return true if the two intervals have the same properties.
#[no_mangle]
pub extern "C" fn intervals_equal(i0: INTERVAL, i1: INTERVAL) -> bool {
    intervals_equal_rust(IntervalRef::new(i0), IntervalRef::new(i1))
}

pub fn intervals_equal_rust(i0: IntervalRef, i1: IntervalRef) -> bool {
    if i0.is_default() && i1.is_default() {
        return true;
    }

    if i0.is_default() || i1.is_default() {
        return false;
    }

    let mut i0_cdr = i0.plist;
    let mut i1_cdr = i1.plist;

    while let (Some(mut i0_cons), Some(i1_cons)) = (i0_cdr.as_cons(), i1_cdr.as_cons()) {
        let i0_sym = i0_cons.car();
        i0_cdr = i0_cons.cdr();

        if let Some(cons) = i0_cdr.as_cons() {
            i0_cons = cons;
        } else {
            return false;
        }

        let mut i1_val = i1.plist;
        while let Some(cons) = i1_val.as_cons() {
            if cons.car().eq(i0_sym) {
                break;
            }

            i1_val = cons.cdr();
            if let Some(cons_1) = i1_val.as_cons() {
                i1_val = cons_1.cdr();
            } else {
                return false;
            }
        }

        // i0 has something i1 doesn't.
        if i1_val.is_nil() || !i1_val.is_cons() {
            return false;
        }

        let i1_val_cons = i1_val.as_cons().unwrap();

        i1_val = i1_val_cons.cdr();

        // i0 and i1 both have sym, but it has different values in each.
        if let Some(i1_val_cons) = i1_val.as_cons() {
            if !i1_val_cons.car().eq(i0_cons.car()) {
                return false;
            }
        } else {
            return false;
        }

        i0_cdr = i0_cons.cdr();
        i1_cdr = i1_cons.cdr();
        if let Some(i1_cons_1) = i1_cdr.as_cons() {
            i1_cdr = i1_cons_1.cdr();
        } else {
            return false;
        }
    }

    // Lengths of the two plists were equal.
    i0_cdr.is_nil() && i1_cdr.is_nil()
}

// Find the succeeding interval (lexicographically) to INTERVAL.
// Sets the `position' field based on that of INTERVAL (see
// find_interval).
#[no_mangle]
pub extern "C" fn next_interval(interval: INTERVAL) -> INTERVAL {
    next_interval_rust(IntervalRef::new(interval)).as_mut()
}

pub fn next_interval_rust(interval: IntervalRef) -> IntervalRef {
    if interval.is_null() {
        return interval;
    }

    let next_position = interval.last_pos();

    let mut i = interval;
    if !i.has_null_right_child() {
        i = IntervalRef::new(i.right);
        while !i.has_null_left_child() {
            i = IntervalRef::new(i.left);
        }
        i.position = next_position;
        return i;
    }

    while !i.has_null_parent() {
        if i.am_left_child() {
            i = i.parent();
            i.position = next_position;
            return i;
        }

        i = i.parent();
    }

    IntervalRef::new(ptr::null_mut())
}

// Find the interval containing text position POSITION in the text
// represented by the interval tree TREE.  POSITION is a buffer
// position (starting from 1) or a string index (starting from 0).
// If POSITION is at the end of the buffer or string,
// return the interval containing the last character.
//
// The `position' field, which is a cache of an interval's position,
// is updated in the interval found.  Other functions (e.g., next_interval)
// will update this cache based on the result of find_interval.
#[no_mangle]
pub extern "C" fn find_interval(tree: INTERVAL, position: libc::ptrdiff_t) -> INTERVAL {
    find_interval_rust(IntervalRef::new(tree), position).as_mut()
}

pub fn find_interval_rust(mut tree: IntervalRef, position: libc::ptrdiff_t) -> IntervalRef {
    if tree.is_null() {
        return tree;
    }

    // The distance from the left edge of the subtree at TREE to POSITION.
    let mut relative_position = position;

    if tree.has_object() {
        let parent = tree.get_object();
        if let Some(buf) = parent.as_buffer() {
            relative_position -= buf.beg();
        }
    }

    assert!(relative_position <= tree.get_total_length());

    tree = IntervalRef::new(unsafe { balance_possible_root_interval(tree.as_mut()) });

    loop {
        assert!(!tree.is_null());
        if relative_position < tree.left_total_length() {
            tree = IntervalRef::new(tree.left);
        } else if !tree.has_null_right_child()
            && relative_position >= tree.get_total_length() - tree.right_total_length()
        {
            relative_position -= tree.get_total_length() - tree.right_total_length();
            tree = IntervalRef::new(tree.right);
        } else {
            tree.position = position - relative_position // left edge of *tree.
	       + tree.left_total_length(); // left edge of this interval.

            break tree;
        }
    }
}
