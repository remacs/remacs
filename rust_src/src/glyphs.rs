use crate::{
    lisp::ExternalPtr,
    numbers::check_range,
    remacs_sys::{glyph_matrix, glyph_row, EmacsInt},
    windows::LispWindowRef,
};

pub type LispGlyphMatrixRef = ExternalPtr<glyph_matrix>;

impl LispGlyphMatrixRef {
    /// Get a pointer to row number ROW.
    pub unsafe fn row_unchecked(self, row: usize) -> LispGlyphRowRef {
        LispGlyphRowRef::new(self.rows.add(row))
    }

    /// Get a pointer to row number ROW. Throws an error if out of range.
    pub fn row(self, row: usize) -> LispGlyphRowRef {
        check_range(row as EmacsInt, 0, self.nrows);
        unsafe { self.row_unchecked(row) }
    }

    pub fn mode_line_height(self) -> i32 {
        if self.is_null() || self.rows.is_null() {
            0
        } else {
            unsafe { (*self.rows.offset((self.nrows - 1) as isize)).height }
        }
    }
    pub fn header_line_height(self) -> i32 {
        if self.is_null() || self.rows.is_null() {
            0
        } else {
            unsafe { (*self.rows).height }
        }
    }

    /// Return a pointer to the first row used for text display
    pub fn first_text_row(self) -> LispGlyphRowRef {
        unsafe {
            if (*self.rows).mode_line_p() {
                LispGlyphRowRef::new(self.rows.offset(1))
            } else {
                LispGlyphRowRef::new(self.rows)
            }
        }
    }

    pub fn bottom_text_row(self, window: LispWindowRef) -> LispGlyphRowRef {
        let ptr = unsafe {
            self.rows
                .offset((self.nrows - if window.wants_mode_line() { 1 } else { 0 }) as isize)
        };
        LispGlyphRowRef::new(ptr)
    }
}

pub type LispGlyphRowRef = ExternalPtr<glyph_row>;
