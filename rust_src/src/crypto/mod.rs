use std;
use std::slice;

use libc::ptrdiff_t;

use md5 as md5_crate;
use sha1;
use sha2::{Digest, Sha224, Sha256, Sha384, Sha512};

use remacs_macros::lisp_fn;

use crate::{
    buffers::{LispBufferOrName, LispBufferRef},
    lisp::LispObject,
    multibyte::LispStringRef,
    remacs_sys::EmacsInt,
    remacs_sys::{extract_data_from_object, make_uninit_string},
    remacs_sys::{Qmd5, Qnil, Qsha1, Qsha224, Qsha256, Qsha384, Qsha512},
    symbols::{symbol_name, LispSymbolRef},
    threads::ThreadState,
};

#[derive(Clone, Copy)]
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

fn hash_alg(algorithm: LispSymbolRef) -> HashAlg {
    match LispObject::from(algorithm) {
        Qmd5 => HashAlg::MD5,
        Qsha1 => HashAlg::SHA1,
        Qsha224 => HashAlg::SHA224,
        Qsha256 => HashAlg::SHA256,
        Qsha384 => HashAlg::SHA384,
        Qsha512 => HashAlg::SHA512,
        _ => {
            let name: LispStringRef = symbol_name(algorithm).into();
            error!("Invalid algorithm arg: {:?}\0", &name.as_slice());
        }
    }
}

/// Return MD5 message digest of OBJECT, a buffer or string.
///
/// A message digest is a cryptographic checksum of a document, and the
/// algorithm to calculate it is defined in RFC 1321.
///
/// The two optional arguments START and END are character positions
/// specifying for which part of OBJECT the message digest should be
/// computed.  If nil or omitted, the digest is computed for the whole
/// OBJECT.
///
/// The MD5 message digest is computed from the result of encoding the
/// text in a coding system, not directly from the internal Emacs form of
/// the text.  The optional fourth argument CODING-SYSTEM specifies which
/// coding system to encode the text with.  It should be the same coding
/// system that you used or will use when actually writing the text into a
/// file.
///
/// If CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If
/// OBJECT is a buffer, the default for CODING-SYSTEM is whatever coding
/// system would be chosen by default for writing this text into a file.
///
/// If OBJECT is a string, the most preferred coding system (see the
/// command `prefer-coding-system') is used.
///
/// If NOERROR is non-nil, silently assume the `raw-text' coding if the
/// guesswork fails.  Normally, an error is signaled in such case.
#[lisp_fn(min = "1")]
pub fn md5(
    object: LispObject,
    start: LispObject,
    end: LispObject,
    coding_system: LispObject,
    noerror: LispObject,
) -> LispObject {
    _secure_hash(
        HashAlg::MD5,
        object,
        start,
        end,
        coding_system,
        noerror,
        Qnil,
    )
}

/// Return the secure hash of OBJECT, a buffer or string.
/// ALGORITHM is a symbol specifying the hash to use:
/// md5, sha1, sha224, sha256, sha384 or sha512.
///
/// The two optional arguments START and END are positions specifying for
/// which part of OBJECT to compute the hash.  If nil or omitted, uses the
/// whole OBJECT.
///
/// The full list of algorithms can be obtained with `secure-hash-algorithms'.
///
/// If BINARY is non-nil, returns a string in binary form.
#[lisp_fn(min = "2")]
pub fn secure_hash(
    algorithm: LispSymbolRef,
    object: LispObject,
    start: LispObject,
    end: LispObject,
    binary: LispObject,
) -> LispObject {
    _secure_hash(hash_alg(algorithm), object, start, end, Qnil, Qnil, binary)
}

fn _secure_hash(
    algorithm: HashAlg,
    object: LispObject,
    start: LispObject,
    end: LispObject,
    coding_system: LispObject,
    noerror: LispObject,
    binary: LispObject,
) -> LispObject {
    type HashFn = fn(&[u8], &mut [u8]);

    let spec = list!(object, start, end, coding_system, noerror);
    let mut start_byte: ptrdiff_t = 0;
    let mut end_byte: ptrdiff_t = 0;
    let input = unsafe { extract_data_from_object(spec, &mut start_byte, &mut end_byte) };

    if input.is_null() {
        error!("secure_hash: failed to extract data from object, aborting!");
    }

    let input_slice = unsafe {
        slice::from_raw_parts(
            input.offset(start_byte) as *mut u8,
            (end_byte - start_byte) as usize,
        )
    };

    let (digest_size, hash_func) = match algorithm {
        HashAlg::MD5 => (MD5_DIGEST_LEN, md5_buffer as HashFn),
        HashAlg::SHA1 => (SHA1_DIGEST_LEN, sha1_buffer as HashFn),
        HashAlg::SHA224 => (SHA224_DIGEST_LEN, sha224_buffer as HashFn),
        HashAlg::SHA256 => (SHA256_DIGEST_LEN, sha256_buffer as HashFn),
        HashAlg::SHA384 => (SHA384_DIGEST_LEN, sha384_buffer as HashFn),
        HashAlg::SHA512 => (SHA512_DIGEST_LEN, sha512_buffer as HashFn),
    };

    let buffer_size = if binary.is_nil() {
        (digest_size * 2) as EmacsInt
    } else {
        digest_size as EmacsInt
    };
    let digest = unsafe { make_uninit_string(buffer_size as EmacsInt) };
    let mut digest_str: LispStringRef = digest.into();
    hash_func(input_slice, digest_str.as_mut_slice());
    if binary.is_nil() {
        hexify_digest_string(digest_str.as_mut_slice(), digest_size);
    }
    digest
}

/// To avoid a copy, buffer is both the source and the destination of
/// this transformation. Buffer must contain len bytes of data and
/// 2*len bytes of space for the final hex string.
fn hexify_digest_string(buffer: &mut [u8], len: usize) {
    static hexdigit: [u8; 16] = *b"0123456789abcdef";
    debug_assert_eq!(
        buffer.len(),
        2 * len,
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
// digest. Additionally, the caller may have been asked to return a
// hex string, in which case dest_buf will be twice as long as the
// digest.

fn md5_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    let output = md5_crate::compute(buffer);
    dest_buf[..output.len()].copy_from_slice(&*output)
}

fn sha1_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    let mut hasher = sha1::Sha1::new();
    hasher.update(buffer);
    let output = hasher.digest().bytes();
    dest_buf[..output.len()].copy_from_slice(&output)
}

/// Given an instance of `Digest`, and `buffer` write its hash to `dest_buf`.
fn sha2_hash_buffer(hasher: impl Digest, buffer: &[u8], dest_buf: &mut [u8]) {
    let mut hasher = hasher;
    hasher.input(buffer);
    let output = hasher.result();
    dest_buf[..output.len()].copy_from_slice(&output)
}

fn sha224_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha224::new(), buffer, dest_buf);
}

fn sha256_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha256::new(), buffer, dest_buf);
}

fn sha384_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha384::new(), buffer, dest_buf);
}

fn sha512_buffer(buffer: &[u8], dest_buf: &mut [u8]) {
    sha2_hash_buffer(Sha512::new(), buffer, dest_buf);
}

/// Return a hash of the contents of BUFFER-OR-NAME.
/// This hash is performed on the raw internal format of the buffer,
/// disregarding any coding systems.  If nil, use the current buffer.
#[lisp_fn(min = "0")]
pub fn buffer_hash(buffer_or_name: Option<LispBufferOrName>) -> LispObject {
    let b = buffer_or_name.map_or_else(ThreadState::current_buffer_unchecked, LispBufferRef::from);
    let mut ctx = sha1::Sha1::new();

    ctx.update(unsafe {
        slice::from_raw_parts(b.beg_addr(), (b.gpt_byte() - b.beg_byte()) as usize)
    });
    if b.gpt_byte() < b.z_byte() {
        ctx.update(unsafe {
            slice::from_raw_parts(
                b.gap_end_addr(),
                b.z_addr() as usize - b.gap_end_addr() as usize,
            )
        });
    }

    let formatted = ctx.digest().to_string();
    let digest = unsafe { make_uninit_string(formatted.len() as EmacsInt) };
    digest
        .as_string()
        .unwrap()
        .as_mut_slice()
        .copy_from_slice(formatted.as_bytes());
    digest
}

include!(concat!(env!("OUT_DIR"), "/crypto_exports.rs"));
