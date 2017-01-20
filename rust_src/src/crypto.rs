extern crate libc;

use std::ptr;
use lisp::LispObject;

extern "C" {
    pub static Qmd5: LispObject;
    pub static Qsha1: LispObject;
    pub static Qsha224: LispObject;
    pub static Qsha256: LispObject;
    pub static Qsha384: LispObject;
    pub static Qsha512: LispObject;
}

fn Fmd5(object: LispObject,
        start: LispObject,
        end: LispObject,
        coding_system: LispObject,
        noerror: LispObject)
        -> LispObject {
    // TODO implement
}

fn Fsecure_hash(algorithm: LispObject,
                object: LispObject,
                start: LispObject,
                end: LispObject,
                binary: LispObject)
                -> LispObject {
    // TODO implement
}

defun!("md5",
       Fmd5,
       Smd5,
       1,
       5,
       ptr::null(),
       "Return MD5 message digest of OBJECT, a buffer or string.

A message digest is a cryptographic checksum of a document, and the
algorithm to calculate it is defined in RFC 1321.

The two optional arguments START and END are character positions
specifying for which part of OBJECT the message digest should be
computed.  If nil or omitted, the digest is computed for the whole
OBJECT.

The MD5 message digest is computed from the result of encoding the
text in a coding system, not directly from the internal Emacs form of
the text.  The optional fourth argument CODING-SYSTEM specifies which
coding system to encode the text with.  It should be the same coding
system that you used or will use when actually writing the text into a
file.

If CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If
OBJECT is a buffer, the default for CODING-SYSTEM is whatever coding
system would be chosen by default for writing this text into a file.

If OBJECT is a string, the most preferred coding system (see the
command `prefer-coding-system') is used.

If NOERROR is non-nil, silently assume the `raw-text' coding if the
guesswork fails.  Normally, an error is signaled in such case.");

defun!("secure-hash",
       Fsecure_hash,
       Ssecure_hash,
       2,
       5,
       ptr::null(),
       "Return the secure hash of OBJECT, a buffer or string.
ALGORITHM is a symbol specifying the hash to use:
md5, sha1, sha224, sha256, sha384, or sha512.

The two optional arguments START and END are positions specifying for
which part of OBJECT to compute the hash.  If nil or omitted, uses the
whole OBJECT.

If BINARY is non-nil, returns a string in binary form.");
