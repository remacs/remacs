use md5;
use sha1;
use sha2::{Sha224, Digest, Sha256, Sha384, Sha512};
use std;
use std::{ptr, slice};
use libc::ptrdiff_t;

use buffers::{LispBufferRef, get_buffer};
use eval::xsignal1;
use libc;
use lisp::{LispObject, LispNumber};
use multibyte::LispStringRef;
use remacs_sys::{error, nsberror, Fcurrent_buffer, EmacsInt, make_uninit_string,
                 make_specified_string};
use remacs_sys::{preferred_coding_system, Fcoding_system_p, code_convert_string,
                 validate_subarray, string_char_to_byte, wrong_type_argument};
use remacs_sys::{current_thread, record_unwind_current_buffer, set_buffer_internal,
                 make_buffer_string, call4};
use remacs_sys::{globals, Fbuffer_file_name, Ffind_operation_coding_system, Flocal_variable_p};
use remacs_sys::{Qmd5, Qsha1, Qsha224, Qsha256, Qsha384, Qsha512, Qstringp, Qraw_text,
                 Qcoding_system_error, Qwrite_region, Qbuffer_file_coding_system};
use remacs_macros::lisp_fn;
use symbols::{symbol_name, fboundp};

enum HashAlg {
    MD5,
    SHA1,
    SHA224,
    SHA256,
    SHA384,
    SHA512,
}

static MD5_DIGEST_LEN: usize = 16;
static SHA1_DIGEST_LEN: usize = 20;
static SHA224_DIGEST_LEN: usize = 224 / 8;
static SHA256_DIGEST_LEN: usize = 256 / 8;
static SHA384_DIGEST_LEN: usize = 384 / 8;
static SHA512_DIGEST_LEN: usize = 512 / 8;

fn hash_alg(algorithm: LispObject) -> HashAlg {
    algorithm.as_symbol_or_error();
    if algorithm.to_raw() == unsafe { Qmd5 } {
        HashAlg::MD5
    } else if algorithm.to_raw() == unsafe { Qsha1 } {
        HashAlg::SHA1
    } else if algorithm.to_raw() == unsafe { Qsha224 } {
        HashAlg::SHA224
    } else if algorithm.to_raw() == unsafe { Qsha256 } {
        HashAlg::SHA256
    } else if algorithm.to_raw() == unsafe { Qsha384 } {
        HashAlg::SHA384
    } else if algorithm.to_raw() == unsafe { Qsha512 } {
        HashAlg::SHA512
    } else {
        let name = symbol_name(algorithm).as_string_or_error();
        unsafe {
            error(b"Invalid algorithm arg: %s\0".as_ptr(), name.as_slice());
        }
    }
}

fn check_coding_system_or_error(coding_system: LispObject, noerror: LispObject) -> LispObject {
    if LispObject::from_raw(unsafe { Fcoding_system_p(coding_system.to_raw()) }).is_nil() {
        /* Invalid coding system. */
        if noerror.is_not_nil() {
            LispObject::from_raw(unsafe { Qraw_text })
        } else {
            xsignal1(
                LispObject::from_raw(unsafe { Qcoding_system_error }),
                coding_system,
            );
        }
    } else {
        coding_system
    }
}

fn get_coding_system_for_string(string: LispStringRef, coding_system: LispObject) -> LispObject {
    if coding_system.is_nil() {
        /* Decide the coding-system to encode the data with. */
        if string.is_multibyte() {
            /* use default, we can't guess correct value */
            LispObject::from_raw(unsafe { preferred_coding_system() })
        } else {
            LispObject::from_raw(unsafe { Qraw_text })
        }
    } else {
        coding_system
    }
}

fn get_coding_system_for_buffer(
    object: LispObject,
    buffer: LispBufferRef,
    start: LispObject,
    end: LispObject,
    start_byte: ptrdiff_t,
    end_byte: ptrdiff_t,
    coding_system: LispObject,
) -> LispObject {
    /* Decide the coding-system to encode the data with.
       See fileio.c:Fwrite-region */
    if coding_system.is_not_nil() {
        return coding_system;
    }
    if LispObject::from_raw(unsafe { globals.f_Vcoding_system_for_write }).is_not_nil() {
        return LispObject::from_raw(unsafe { globals.f_Vcoding_system_for_write });
    }
    if LispObject::from_raw(buffer.buffer_file_coding_system).is_nil() ||
        LispObject::from_raw(unsafe {
            Flocal_variable_p(
                Qbuffer_file_coding_system,
                LispObject::constant_nil().to_raw(),
            )
        }).is_nil()
    {
        if LispObject::from_raw(buffer.enable_multibyte_characters).is_nil() {
            return LispObject::from_raw(unsafe { Qraw_text });
        }
    }
    if LispObject::from_raw(unsafe { Fbuffer_file_name(object.to_raw()) }).is_not_nil() {
        /* Check file-coding-system-alist. */
        let mut args = [
            unsafe { Qwrite_region },
            start.to_raw(),
            end.to_raw(),
            unsafe { Fbuffer_file_name(object.to_raw()) },
        ];
        let val = LispObject::from_raw(unsafe {
            Ffind_operation_coding_system(4, args.as_mut_ptr())
        });
        if val.is_cons() && val.as_cons_or_error().cdr().is_not_nil() {
            return val.as_cons_or_error().cdr();
        }
    }
    if LispObject::from_raw(buffer.buffer_file_coding_system).is_not_nil() {
        /* If we still have not decided a coding system, use the
           default value of buffer-file-coding-system. */
        return LispObject::from_raw(buffer.buffer_file_coding_system);
    }
    if fboundp(LispObject::from_raw(
        unsafe { globals.f_Vselect_safe_coding_system_function },
    )).is_not_nil()
    {
        /* Confirm that VAL can surely encode the current region. */
        return LispObject::from_raw(unsafe {
            call4(
                globals.f_Vselect_safe_coding_system_function,
                LispObject::from_natnum(start_byte as EmacsInt).to_raw(),
                LispObject::from_natnum(end_byte as EmacsInt).to_raw(),
                coding_system.to_raw(),
                LispObject::constant_nil().to_raw(),
            )
        });
    }
    LispObject::constant_nil()
}

fn get_input_from_string(
    object: LispObject,
    string: LispStringRef,
    start: LispObject,
    end: LispObject,
) -> LispObject {
    let size: ptrdiff_t;
    let start_byte: ptrdiff_t;
    let end_byte: ptrdiff_t;
    let mut start_char: ptrdiff_t = 0;
    let mut end_char: ptrdiff_t = 0;

    size = string.len_bytes();
    unsafe {
        validate_subarray(
            object.to_raw(),
            start.to_raw(),
            end.to_raw(),
            size,
            &mut start_char,
            &mut end_char,
        );
    }
    start_byte = if start_char == 0 {
        0
    } else {
        unsafe { string_char_to_byte(object.to_raw(), start_char) }
    };
    end_byte = if end_char == size {
        string.len_bytes()
    } else {
        unsafe { string_char_to_byte(object.to_raw(), end_char) }
    };
    if start_byte == 0 && end_byte == size {
        object
    } else {
        LispObject::from_raw(unsafe {
            make_specified_string(
                string.const_sdata_ptr().offset(start_byte),
                -1 as ptrdiff_t,
                end_byte - start_byte,
                string.is_multibyte(),
            )
        })
    }
}

fn get_input_from_buffer(
    buffer: LispBufferRef,
    start: LispObject,
    end: LispObject,
    start_byte: &mut ptrdiff_t,
    end_byte: &mut ptrdiff_t,
) -> LispObject {
    let prev_buffer = unsafe { (*current_thread).m_current_buffer };
    unsafe { record_unwind_current_buffer() };
    unsafe { set_buffer_internal(buffer.as_ptr() as *const _ as *const libc::c_void) };
    *start_byte = if start.is_nil() {
        buffer.begv
    } else {
        match start.as_number_coerce_marker_or_error() {
            LispNumber::Fixnum(n) => n as ptrdiff_t,
            LispNumber::Float(n) => n as ptrdiff_t,
        }
    };
    *end_byte = if end.is_nil() {
        buffer.zv
    } else {
        match end.as_number_coerce_marker_or_error() {
            LispNumber::Fixnum(n) => n as ptrdiff_t,
            LispNumber::Float(n) => n as ptrdiff_t,
        }
    };
    if start_byte > end_byte {
        std::mem::swap(start_byte, end_byte);
    }
    //if !(buffer.begv <= start_byte && end_byte <= buffer.zv) {
    //    args_out_of_range(start, end);
    //}
    let string = LispObject::from_raw(unsafe { make_buffer_string(*start_byte, *end_byte, false) });
    unsafe { set_buffer_internal(prev_buffer) };
    // TODO: this needs to be std::mem::size_of<specbinding>()
    unsafe { (*current_thread).m_specpdl_ptr.offset(-40) };
    string
}

fn get_input(
    object: LispObject,
    string: &mut Option<LispStringRef>,
    buffer: &Option<LispBufferRef>,
    start: LispObject,
    end: LispObject,
    coding_system: LispObject,
    noerror: LispObject,
) -> LispStringRef {
    if object.is_string() {
        if string.unwrap().is_multibyte() {
            let coding_system = check_coding_system_or_error(
                get_coding_system_for_string(string.unwrap(), coding_system),
                noerror,
            );
            *string = Some(
                LispObject::from_raw(unsafe {
                    code_convert_string(
                        object.to_raw(),
                        coding_system.to_raw(),
                        LispObject::constant_nil().to_raw(),
                        true,
                        false,
                        true,
                    )
                }).as_string_or_error(),
            )
        }
        get_input_from_string(object, string.unwrap(), start, end).as_string_or_error()
    } else if object.is_buffer() {
        let mut start_byte: ptrdiff_t = 0;
        let mut end_byte: ptrdiff_t = 0;
        let s = get_input_from_buffer(buffer.unwrap(), start, end, &mut start_byte, &mut end_byte);
        let ss = s.as_string_or_error();
        if ss.is_multibyte() {
            let coding_system = check_coding_system_or_error(
                get_coding_system_for_buffer(
                    object,
                    buffer.unwrap(),
                    start,
                    end,
                    start_byte,
                    end_byte,
                    coding_system,
                ),
                noerror,
            );
            LispObject::from_raw(unsafe {
                code_convert_string(
                    s.to_raw(),
                    coding_system.to_raw(),
                    LispObject::constant_nil().to_raw(),
                    true,
                    false,
                    false,
                )
            }).as_string_or_error()
        } else {
            ss
        }
    } else {
        unsafe {
            wrong_type_argument(Qstringp, object.to_raw());
        }
    }
}

/// Return the secure hash of OBJECT, a buffer or string.
/// ALGORITHM is a symbol specifying the hash to use:
/// md5, sha1, sha224, sha256, sha384 or sha512.
///
/// The two optional arguments START and END are positions specifying for
/// which part of OBJECT to compute the hash.  If nil or omitted, uses the
/// whole OBJECT.
///
/// If BINARY is non-nil, returns a string in binary form.
#[lisp_fn(min = "1")]
fn md5(
    object: LispObject,
    start: LispObject,
    end: LispObject,
    coding_system: LispObject,
    noerror: LispObject,
) -> LispObject {
    let mut string = object.as_string();
    let buffer = object.as_buffer();
    let input = get_input(
        object,
        &mut string,
        &buffer,
        start,
        end,
        coding_system,
        noerror,
    );
    _secure_hash(HashAlg::MD5, input.as_slice(), true)
}

/// Return the secure hash of OBJECT, a buffer or string.
/// ALGORITHM is a symbol specifying the hash to use:
/// md5, sha1, sha224, sha256, sha384 or sha512.
///
/// The two optional arguments START and END are positions specifying for
/// which part of OBJECT to compute the hash.  If nil or omitted, uses the
/// whole OBJECT.
///
/// If BINARY is non-nil, returns a string in binary form.
#[lisp_fn(min = "2")]
fn secure_hash(
    algorithm: LispObject,
    object: LispObject,
    start: LispObject,
    end: LispObject,
    binary: LispObject,
) -> LispObject {
    let mut string = object.as_string();
    let buffer = object.as_buffer();
    let input = get_input(
        object,
        &mut string,
        &buffer,
        start,
        end,
        LispObject::constant_nil(),
        LispObject::constant_nil(),
    );
    _secure_hash(hash_alg(algorithm), input.as_slice(), binary.is_nil())
}

fn _secure_hash(algorithm: HashAlg, input: &[u8], hex: bool) -> LispObject {
    let digest_size: usize;
    let hash_func: unsafe fn(&[u8], &mut [u8]);
    match algorithm {
        HashAlg::MD5 => {
            digest_size = MD5_DIGEST_LEN;
            hash_func = md5_buffer;
        }
        HashAlg::SHA1 => {
            digest_size = SHA1_DIGEST_LEN;
            hash_func = sha1_buffer;
        }
        HashAlg::SHA224 => {
            digest_size = SHA224_DIGEST_LEN;
            hash_func = sha224_buffer;
        }
        HashAlg::SHA256 => {
            digest_size = SHA256_DIGEST_LEN;
            hash_func = sha256_buffer;
        }
        HashAlg::SHA384 => {
            digest_size = SHA384_DIGEST_LEN;
            hash_func = sha384_buffer;
        }
        HashAlg::SHA512 => {
            digest_size = SHA512_DIGEST_LEN;
            hash_func = sha512_buffer;
        }
    }

    let buffer_size = if hex {
        (digest_size * 2) as EmacsInt
    } else {
        digest_size as EmacsInt
    };
    let digest = LispObject::from_raw(unsafe { make_uninit_string(buffer_size as i64) });
    let digest_str = digest.as_string_or_error();
    unsafe {
        // we can call this safely because we know that we made
        // digest's buffer long enough
        hash_func(input, digest_str.as_mut_slice());
    }
    if hex {
        hexify_digest_string(digest_str.as_mut_slice(), digest_size);
    }
    digest
}

/// To avoid a copy, buffer is both the source and the destination of
/// this transformation. Buffer must contain len bytes of data and
/// 2*len bytes of space for the final hex string.
fn hexify_digest_string(buffer: &mut [u8], len: usize) {
    static hexdigit: [u8; 16] = *b"0123456789abcdef";
    debug_assert!(
        buffer.len() == 2 * len,
        "buffer must be long enough to hold 2*len hex digits"
    );
    for i in (0..len).rev() {
        let v = buffer[i];
        buffer[2 * i] = hexdigit[(v >> 4) as usize];
        buffer[2 * i + 1] = hexdigit[(v & 0xf) as usize];
    }
}

// For the following hash functions, the caller must ensure that the
// destination buffer is at least long enough to hold the
// digest. Additionall, the caller may have been asked to return a hex
// string, in which case dest_buf will be twice as long as the digest.
// Thus, these functions are unsafe.

unsafe fn md5_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    let output = md5::compute(buffer);
    ptr::copy_nonoverlapping(output.as_ptr(), dest_buf.as_ptr() as *mut u8, output.len());
}

unsafe fn sha1_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    let mut hasher = sha1::Sha1::new();
    hasher.update(buffer);
    let output = hasher.digest().bytes();
    ptr::copy_nonoverlapping(output.as_ptr(), dest_buf.as_ptr() as *mut u8, output.len());
}

/// Given an instance of `Digest`, and `buffer` write its hash to `dest_buf`.
unsafe fn sha2_hash_buffer<D>(hasher: D, buffer: &[u8], dest_buf: &mut [u8])
where
    D: Digest,
{
    let mut hasher = hasher;
    hasher.input(buffer);
    let output = hasher.result();
    ptr::copy_nonoverlapping(output.as_ptr(), dest_buf.as_ptr() as *mut u8, output.len());
}

unsafe fn sha224_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha224::new(), buffer, dest_buf);
}

unsafe fn sha256_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha256::new(), buffer, dest_buf);
}

unsafe fn sha384_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha384::new(), buffer, dest_buf);
}

unsafe fn sha512_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha512::new(), buffer, dest_buf);
}

/// Return a hash of the contents of BUFFER-OR-NAME.
/// This hash is performed on the raw internal format of the buffer,
/// disregarding any coding systems.  If nil, use the current buffer.
#[lisp_fn(min = "0")]
fn buffer_hash(buffer_or_name: LispObject) -> LispObject {
    let buffer = if buffer_or_name.is_nil() {
        LispObject::from_raw(unsafe { Fcurrent_buffer() })
    } else {
        get_buffer(buffer_or_name)
    };

    if buffer.is_nil() {
        unsafe { nsberror(buffer_or_name.to_raw()) };
    }
    let b = buffer.as_vectorlike().unwrap().as_buffer().unwrap();
    let mut ctx = sha1::Sha1::new();

    ctx.update(unsafe {
        slice::from_raw_parts(b.beg_addr(), (b.gpt_byte() - b.beg_byte()) as usize)
    });
    if b.gpt_byte() < b.z_byte() {
        ctx.update(unsafe {
            slice::from_raw_parts(
                b.gap_end_addr(),
                (b.z_addr() as usize - b.gap_end_addr() as usize),
            )
        });
    }

    let formatted = ctx.digest().to_string();
    let digest = LispObject::from_raw(unsafe { make_uninit_string(formatted.len() as EmacsInt) });
    digest.as_string().unwrap().as_mut_slice().copy_from_slice(
        formatted
            .as_bytes(),
    );
    digest
}
