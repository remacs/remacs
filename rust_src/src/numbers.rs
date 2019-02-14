//! Functions operating on numbers.

use std::cmp;
use std::sync::Mutex;

use rand::{rngs::StdRng, FromEntropy, Rng, SeedableRng};

use remacs_macros::lisp_fn;

use crate::{
    hashtable::LispHashTableRef,
    lisp::{LispObject, LispStructuralEqual},
    remacs_sys::{
        equal_kind, EmacsDouble, EmacsInt, EmacsUint, Lisp_Bits, Lisp_Type, EMACS_INT_MAX, INTMASK,
        USE_LSB_TAG,
    },
    remacs_sys::{Qinteger_or_marker_p, Qintegerp, Qnumber_or_marker_p, Qwholenump},
};

lazy_static! {
    static ref RNG: Mutex<StdRng> = Mutex::new(StdRng::from_entropy());
}

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> Lisp_Bits::INTTYPEBITS as u32;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

// Fixnum(Integer) support (LispType == Lisp_Int0 | Lisp_Int1 == 2 | 6(LSB) )

/// Fixnums are inline integers that fit directly into Lisp's tagged word.
/// There's two `LispType` variants to provide an extra bit.

/// Natnums(natural number) are the non-negative fixnums.
/// There were special branches in the original code for better performance.
/// However they are unified into the fixnum logic under LSB mode.
/// TODO: Recheck these logic in original C code.

impl LispObject {
    pub fn from_fixnum(n: EmacsInt) -> LispObject {
        debug_assert!(MOST_NEGATIVE_FIXNUM <= n && n <= MOST_POSITIVE_FIXNUM);
        Self::from_fixnum_truncated(n)
    }

    pub fn from_fixnum_truncated(n: EmacsInt) -> LispObject {
        let o = if USE_LSB_TAG {
            (n << Lisp_Bits::INTTYPEBITS) as EmacsUint + Lisp_Type::Lisp_Int0 as EmacsUint
        } else {
            (n & INTMASK) as EmacsUint + ((Lisp_Type::Lisp_Int0 as EmacsUint) << Lisp_Bits::VALBITS)
        };
        LispObject::from_C(o as EmacsInt)
    }

    pub fn is_fixnum(self) -> bool {
        let ty = self.get_type();
        (ty as u8 & ((Lisp_Type::Lisp_Int0 as u8) | !(Lisp_Type::Lisp_Int1 as u8)))
            == Lisp_Type::Lisp_Int0 as u8
    }

    pub fn force_fixnum(self) -> EmacsInt {
        unsafe { self.to_fixnum_unchecked() }
    }

    pub fn as_fixnum(self) -> Option<EmacsInt> {
        if self.is_fixnum() {
            Some(unsafe { self.to_fixnum_unchecked() })
        } else {
            None
        }
    }

    pub fn as_fixnum_or_error(self) -> EmacsInt {
        if self.is_fixnum() {
            unsafe { self.to_fixnum_unchecked() }
        } else {
            wrong_type!(Qintegerp, self)
        }
    }
}

impl LispObject {
    /// Convert a positive integer into its LispObject representation.
    ///
    /// This is also the function to use when translating `XSETFASTINT`
    /// from Emacs C.
    // TODO: the C claims that make_natnum is faster, but it does the same
    // thing as make_number when USE_LSB_TAG is 1, which it is for us. We
    // should remove this in favour of make_number.
    pub fn from_natnum(n: EmacsUint) -> LispObject {
        debug_assert!(n <= (MOST_POSITIVE_FIXNUM as EmacsUint));
        LispObject::from_fixnum_truncated(n as EmacsInt)
    }

    pub fn is_natnum(self) -> bool {
        self.as_fixnum().map_or(false, |i| i >= 0)
    }

    pub fn as_natnum(self) -> Option<EmacsUint> {
        if self.is_natnum() {
            Some(unsafe { self.to_fixnum_unchecked() as EmacsUint })
        } else {
            None
        }
    }

    pub fn as_natnum_or_error(self) -> EmacsUint {
        self.as_natnum()
            .unwrap_or_else(|| wrong_type!(Qwholenump, self))
    }
}

impl LispObject {
    pub fn int_or_float_from_fixnum(n: EmacsInt) -> LispObject {
        if n < MOST_NEGATIVE_FIXNUM || n > MOST_POSITIVE_FIXNUM {
            Self::from_float(n as f64)
        } else {
            Self::from_fixnum(n)
        }
    }

    pub fn fixnum_overflow(n: EmacsInt) -> bool {
        n < MOST_NEGATIVE_FIXNUM || n > MOST_POSITIVE_FIXNUM
    }

    pub unsafe fn to_fixnum_unchecked(self) -> EmacsInt {
        let raw = self.to_C();
        if USE_LSB_TAG {
            raw >> Lisp_Bits::INTTYPEBITS
        } else {
            raw & INTMASK
        }
    }

    pub fn as_fixnum_coerce_marker_or_error(self) -> EmacsInt {
        if let Some(n) = self.as_fixnum() {
            n
        } else if let Some(m) = self.as_marker() {
            m.charpos_or_error() as EmacsInt
        } else {
            wrong_type!(Qinteger_or_marker_p, self);
        }
    }

    /// TODO: Bignum support? (Current Emacs doesn't have it)
    pub fn is_integer(self) -> bool {
        self.is_fixnum()
    }
}

#[derive(Clone, Copy)]
pub enum LispNumber {
    Fixnum(EmacsInt),
    Float(EmacsDouble),
}

pub trait IsLispNatnum {
    fn check_natnum(self);
}

impl IsLispNatnum for EmacsInt {
    fn check_natnum(self) {
        if self < 0 {
            wrong_type!(Qwholenump, self);
        }
    }
}

/// Check if NUM is within range [FROM..TO]
pub fn check_range(num: impl Into<EmacsInt>, from: impl Into<EmacsInt>, to: impl Into<EmacsInt>) {
    let num: EmacsInt = num.into();
    let from: EmacsInt = from.into();
    let to: EmacsInt = to.into();
    if !(from <= num && num <= to) {
        args_out_of_range!(
            num,
            if from < 0 && from < MOST_NEGATIVE_FIXNUM {
                MOST_NEGATIVE_FIXNUM
            } else {
                from
            },
            cmp::min(to, MOST_POSITIVE_FIXNUM)
        )
    }
}

impl LispNumber {
    pub fn to_fixnum(&self) -> EmacsInt {
        match *self {
            LispNumber::Fixnum(v) => v,
            LispNumber::Float(v) => v as EmacsInt,
        }
    }
}

impl LispStructuralEqual for EmacsInt {
    fn equal(
        &self,
        other: Self,
        _equal_kind: equal_kind::Type,
        _depth: i32,
        _ht: &mut LispHashTableRef,
    ) -> bool {
        *self == other
    }
}

impl From<EmacsInt> for LispNumber {
    fn from(n: EmacsInt) -> Self {
        LispNumber::Fixnum(n)
    }
}

impl From<LispObject> for LispNumber {
    fn from(o: LispObject) -> Self {
        o.as_number_coerce_marker()
            .unwrap_or_else(|| wrong_type!(Qnumber_or_marker_p, o))
    }
}

impl From<LispObject> for Option<LispNumber> {
    fn from(o: LispObject) -> Self {
        if let Some(n) = o.as_fixnum() {
            Some(LispNumber::Fixnum(n))
        } else if let Some(f) = o.as_float() {
            Some(LispNumber::Float(f))
        } else if let Some(m) = o.as_marker() {
            Some(LispNumber::Fixnum(m.charpos_or_error() as EmacsInt))
        } else {
            None
        }
    }
}

impl From<LispNumber> for LispObject {
    fn from(n: LispNumber) -> LispObject {
        match n {
            LispNumber::Fixnum(v) => v.into(),
            LispNumber::Float(v) => LispObject::from_float(v),
        }
    }
}

impl LispObject {
    pub fn is_number(self) -> bool {
        self.is_fixnum() || self.is_float()
    }

    /*
    pub fn as_number_or_error(self) -> LispNumber {
        if let Some(n) = self.as_fixnum() {
            LispNumber::Fixnum(n)
        } else if let Some(f) = self.as_float() {
            LispNumber::Float(f)
        } else {
            wrong_type!(Qnumberp, self)
        }
    }
    */

    pub fn as_number_coerce_marker(self) -> Option<LispNumber> {
        self.into()
    }

    pub fn as_number_coerce_marker_or_error(self) -> LispNumber {
        self.into()
    }
}

/// Return t if OBJECT is a floating point number.
#[lisp_fn]
pub fn floatp(object: LispObject) -> bool {
    object.is_float()
}

/// Return t if OBJECT is an integer.
#[lisp_fn]
pub fn integerp(object: LispObject) -> bool {
    object.is_integer()
}

/// Return t if OBJECT is an integer or a marker (editor pointer).
#[lisp_fn]
pub fn integer_or_marker_p(object: LispObject) -> bool {
    object.is_marker() || object.is_integer()
}

/// Return t if OBJECT is a non-negative integer.
#[lisp_fn]
pub fn natnump(object: LispObject) -> bool {
    object.is_natnum()
}

/// Return t if OBJECT is a number (floating point or integer).
#[lisp_fn]
pub fn numberp(object: LispObject) -> bool {
    object.is_number()
}

/// Return t if OBJECT is a number or a marker (editor pointer).
#[lisp_fn]
pub fn number_or_marker_p(object: LispObject) -> bool {
    object.is_number() || object.is_marker()
}

/// Return a pseudo-random number.
/// All integers representable in Lisp, i.e. between `most-negative-fixnum'
/// and `most-positive-fixnum', inclusive, are equally likely.
///
/// With positive integer LIMIT, return random number in interval [0,LIMIT).
/// With argument t, set the random number seed from the system's entropy
/// pool if available, otherwise from less-random volatile data such as the time.
/// With a string argument, set the seed based on the string's contents.
/// Other values of LIMIT are ignored.
///
/// See Info node `(elisp)Random Numbers' for more details.
// NOTE(db48x): does not return an EmacsInt, because it relies on the
// truncating behavior of from_fixnum_truncated.
#[lisp_fn(min = "0")]
pub fn random(limit: LispObject) -> LispObject {
    let mut rng = RNG.lock().unwrap();
    if limit.is_t() {
        *rng = StdRng::from_entropy();
    } else if let Some(s) = limit.as_string() {
        let mut seed = [0; 32];
        let mut values: Vec<u8> = s.as_slice().to_vec();
        values.resize(32, 0);
        seed.copy_from_slice(&values.as_slice());
        *rng = StdRng::from_seed(seed);
    }

    if let Some(limit) = limit.as_fixnum() {
        // Return the remainder, except reject the rare case where
        // get_random returns a number so close to INTMASK that the
        // remainder isn't random.
        loop {
            let val: EmacsInt = rng.gen();
            let remainder = val.abs() % limit;
            if val - remainder <= INTMASK - limit + 1 {
                return remainder.into();
            }
        }
    } else {
        LispObject::from_fixnum_truncated(rng.gen())
    }
}

include!(concat!(env!("OUT_DIR"), "/numbers_exports.rs"));
