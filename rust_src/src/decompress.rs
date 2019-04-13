//! Interface to zlib.
use std::cmp::min;
use std::io::prelude::Read;
use std::slice;

use flate2::read::{DeflateDecoder, GzDecoder, ZlibDecoder};
use remacs_macros::lisp_fn;

use crate::{
    buffers::validate_region_rust,
    lisp::LispObject,
    remacs_sys::{
        del_range_2, insert_from_gap, make_gap, maybe_quit, modify_text, move_gap_both,
        signal_after_change, update_compositions, CHECK_HEAD,
    },
    threads::ThreadState,
};

/// Return t if zlib decompression is available in this instance of Emacs.
#[lisp_fn]
pub fn zlib_available_p() -> bool {
    true
}

fn create_buffer_decoder<'a>(buffer: &'a [u8]) -> Box<Read + 'a> {
    let magic_number = buffer[0];

    match magic_number {
        // Zlib
        0x78 => Box::new(ZlibDecoder::new(buffer)),
        // Gzlib
        0x1F => Box::new(GzDecoder::new(buffer)),
        // Assume the data is raw, if neither zlib nor gzib header can be found.
        _ => Box::new(DeflateDecoder::new(buffer)),
    }
}

/// Decompress a gzip- or zlib-compressed region.
/// Replace the text in the region by the decompressed data.
/// On failure, return nil and leave the data in place.
/// This function can be called only in unibyte buffers.
#[lisp_fn]
pub fn zlib_decompress_region(start: LispObject, end: LispObject) -> bool {
    let (start, end) = validate_region_rust(start, end);

    let mut current_buffer = ThreadState::current_buffer_unchecked();

    if current_buffer.multibyte_characters_enabled() {
        error!("This function can be called only in unibyte buffers");
    };

    // Empty region, decompress failed.
    if start == end {
        return false;
    }

    unsafe {
        // Do the following before manipulating the gap.
        modify_text(start, end);

        move_gap_both(end, end);
    }

    // Insert the decompressed data at the end of the compressed data.
    let charpos = end;
    let bytepos = current_buffer.charpos_to_bytepos(end);
    let old_pt = current_buffer.pt;
    current_buffer.set_pt_both(charpos, bytepos);

    let compressed_buffer = unsafe {
        slice::from_raw_parts(current_buffer.byte_pos_addr(start), (end - start) as usize)
    };

    // The decompressor
    let mut decoder = create_buffer_decoder(compressed_buffer);

    let mut decompressed_bytes: isize = 0;

    loop {
        let avail_out: isize = 16 * 1024;

        let old_gap_size = current_buffer.gap_size();

        if old_gap_size < avail_out {
            unsafe { make_gap(avail_out - old_gap_size) };
        }

        let new_gap_size = avail_out;

        let gap_writer = unsafe {
            slice::from_raw_parts_mut(current_buffer.gap_start_addr(), new_gap_size as usize)
        };

        match decoder.read(gap_writer) {
            // Decompress all data finished.
            Ok(0) => {
                // Delete the compressed data.
                unsafe {
                    del_range_2(
                        start, start, // byte, char offsets the same
                        end, end, false,
                    );
                    signal_after_change(start, end - start, decompressed_bytes);

                    update_compositions(start, start, CHECK_HEAD as i32);
                };
                return true;
            }

            // Decompress one batch data successfully.
            // Continue to decompress the remaining data.
            Ok(decompressed) => {
                let decompressed = decompressed as isize;
                unsafe { insert_from_gap(decompressed, decompressed, false) };

                decompressed_bytes += decompressed;

                unsafe { maybe_quit() };
            }

            // Decompress failed.
            _ => {
                // Delete any uncompressed data already inserted on error, but
                // without calling the change hooks.

                let data_orig = start;
                let data_start = end;
                let data_end = end + decompressed_bytes;

                unsafe {
                    del_range_2(
                        data_start, data_start, // byte, char offsets the same
                        data_end, data_end, false,
                    );
                    update_compositions(data_start, data_start, CHECK_HEAD as i32);
                    // "Balance" the before-change-functions call, which would
                    // otherwise be left "hanging".
                    signal_after_change(data_orig, data_start - data_orig, data_start - data_orig);
                };

                // Put point where it was, or if the buffer has shrunk because the
                // compressed data is bigger than the uncompressed, at
                // point-max.
                let charpos = min(old_pt, current_buffer.zv);
                let bytepos = current_buffer.charpos_to_bytepos(charpos);
                current_buffer.set_pt_both(charpos, bytepos);

                return false;
            }
        };
    }
}

include!(concat!(env!("OUT_DIR"), "/decompress_exports.rs"));
