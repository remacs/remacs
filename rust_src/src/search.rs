//! String search routines

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::{looking_at_1, match_limit, search_command, string_match_1},
};

/// Return t if text after point matches regular expression REGEXP.
/// This function modifies the match data that `match-beginning',
/// `match-end' and `match-data' access; save and restore the match
/// data if you want to preserve them.
#[lisp_fn]
pub fn looking_at(regexp: LispObject) -> LispObject {
    unsafe { looking_at_1(regexp, false) }
}

/// Return t if text after point matches regular expression REGEXP.
/// Find the longest match, in accord with Posix regular expression rules.
/// This function modifies the match data that `match-beginning',
/// `match-end' and `match-data' access; save and restore the match
/// data if you want to preserve them.
#[lisp_fn]
pub fn posix_looking_at(regexp: LispObject) -> LispObject {
    unsafe { looking_at_1(regexp, true) }
}

/// Return index of start of first match for REGEXP in STRING, or nil.
/// Matching ignores case if `case-fold-search' is non-nil.
/// If third arg START is non-nil, start search at that index in STRING.
/// For index of first char beyond the match, do (match-end 0).
/// `match-end' and `match-beginning' also give indices of substrings
/// matched by parenthesis constructs in the pattern.
///
/// You can use the function `match-string' to extract the substrings
/// matched by the parenthesis constructions in REGEXP.
#[lisp_fn(min = "2")]
pub fn string_match(regexp: LispObject, string: LispObject, start: LispObject) -> LispObject {
    unsafe { string_match_1(regexp, string, start, false) }
}

/// Return index of start of first match for REGEXP in STRING, or nil.
/// Find the longest match, in accord with Posix regular expression rules.
/// Case is ignored if `case-fold-search' is non-nil in the current buffer.
/// If third arg START is non-nil, start search at that index in STRING.
/// For index of first char beyond the match, do (match-end 0).
/// `match-end' and `match-beginning' also give indices of substrings
/// matched by parenthesis constructs in the pattern.
#[lisp_fn(min = "2")]
pub fn posix_string_match(regexp: LispObject, string: LispObject, start: LispObject) -> LispObject {
    unsafe { string_match_1(regexp, string, start, true) }
}

/// Search backward from point for STRING.
/// Set point to the beginning of the occurrence found, and return point.
/// An optional second argument bounds the search; it is a buffer position.
///   The match found must not begin before that position.  A value of nil
///   means search to the beginning of the accessible portion of the buffer.
/// Optional third argument, if t, means if fail just return nil (no error).
///   If not nil and not t, position at limit of search and return nil.
/// Optional fourth argument COUNT, if a positive number, means to search
///   for COUNT successive occurrences.  If COUNT is negative, search
///   forward, instead of backward, for -COUNT occurrences.  A value of
///   nil means the same as 1.
/// With COUNT positive, the match found is the COUNTth to last one (or
///   last, if COUNT is 1 or nil) in the buffer located entirely before
///   the origin of the search; correspondingly with COUNT negative.
///
/// Search case-sensitivity is determined by the value of the variable
/// `case-fold-search', which see.
///
/// See also the functions `match-beginning', `match-end' and `replace-match'.
#[lisp_fn(min = "1", intspec = "MSearch backward: ")]
pub fn search_backward(
    string: LispObject,
    bound: LispObject,
    noerror: LispObject,
    count: LispObject,
) -> LispObject {
    unsafe { search_command(string, bound, noerror, count, -1, 0, false) }
}

/// Search forward from point for STRING.
/// Set point to the end of the occurrence found, and return point.
/// An optional second argument bounds the search; it is a buffer position.
///   The match found must not end after that position.  A value of nil
///   means search to the end of the accessible portion of the buffer.
/// Optional third argument, if t, means if fail just return nil (no error).
///   If not nil and not t, move to limit of search and return nil.
/// Optional fourth argument COUNT, if a positive number, means to search
///   for COUNT successive occurrences.  If COUNT is negative, search
///   backward, instead of forward, for -COUNT occurrences.  A value of
///   nil means the same as 1.
/// With COUNT positive, the match found is the COUNTth one (or first,
///   if COUNT is 1 or nil) in the buffer located entirely after the
///   origin of the search; correspondingly with COUNT negative.
///
/// Search case-sensitivity is determined by the value of the variable
/// `case-fold-search', which see.
///
/// See also the functions `match-beginning', `match-end' and `replace-match'.
#[lisp_fn(min = "1", intspec = "MSearch: ")]
pub fn search_forward(
    string: LispObject,
    bound: LispObject,
    noerror: LispObject,
    count: LispObject,
) -> LispObject {
    unsafe { search_command(string, bound, noerror, count, 1, 0, false) }
}

/// Search backward from point for regular expression REGEXP.
/// This function is almost identical to `re-search-forward', except that
/// by default it searches backward instead of forward, and the sign of
/// COUNT also indicates exactly the opposite searching direction.
///
/// See `re-search-forward' for details.
#[lisp_fn(min = "1", intspec = "sRE search backward: ")]
pub fn re_search_backward(
    regexp: LispObject,
    bound: LispObject,
    noerror: LispObject,
    count: LispObject,
) -> LispObject {
    unsafe { search_command(regexp, bound, noerror, count, -1, 1, false) }
}

/// Search forward from point for regular expression REGEXP.
/// Set point to the end of the occurrence found, and return point.
/// The optional second argument BOUND is a buffer position that bounds
///   the search.  The match found must not end after that position.  A
///   value of nil means search to the end of the accessible portion of
///   the buffer.
/// The optional third argument NOERROR indicates how errors are handled
///   when the search fails.  If it is nil or omitted, emit an error; if
///   it is t, simply return nil and do nothing; if it is neither nil nor
///   t, move to the limit of search and return nil.
/// The optional fourth argument COUNT is a number that indicates the
///   search direction and the number of occurrences to search for.  If it
///   is positive, search forward for COUNT successive occurrences; if it
///   is negative, search backward, instead of forward, for -COUNT
///   occurrences.  A value of nil means the same as 1.
/// With COUNT positive/negative, the match found is the COUNTth/-COUNTth
///   one in the buffer located entirely after/before the origin of the
///   search.
///
/// Search case-sensitivity is determined by the value of the variable
/// `case-fold-search', which see.
///
/// See also the functions `match-beginning', `match-end', `match-string',
/// and `replace-match'.
#[lisp_fn(min = "1", intspec = "sRE search: ")]
pub fn re_search_forward(
    regexp: LispObject,
    bound: LispObject,
    noerror: LispObject,
    count: LispObject,
) -> LispObject {
    unsafe { search_command(regexp, bound, noerror, count, 1, 1, false) }
}

/// Search backward from point for match for regular expression REGEXP.
/// Find the longest match in accord with Posix regular expression rules.
/// Set point to the beginning of the occurrence found, and return point.
/// An optional second argument bounds the search; it is a buffer position.
///   The match found must not begin before that position.  A value of nil
///   means search to the beginning of the accessible portion of the buffer.
/// Optional third argument, if t, means if fail just return nil (no error).
///   If not nil and not t, position at limit of search and return nil.
/// Optional fourth argument COUNT, if a positive number, means to search
///   for COUNT successive occurrences.  If COUNT is negative, search
///   forward, instead of backward, for -COUNT occurrences.  A value of
///   nil means the same as 1.
/// With COUNT positive, the match found is the COUNTth to last one (or
///   last, if COUNT is 1 or nil) in the buffer located entirely before
///   the origin of the search; correspondingly with COUNT negative.
///
/// Search case-sensitivity is determined by the value of the variable
/// `case-fold-search', which see.
///
/// See also the functions `match-beginning', `match-end', `match-string',
/// and `replace-match'.
#[lisp_fn(min = "1", intspec = "sPosix search backward: ")]
pub fn posix_search_backward(
    regexp: LispObject,
    bound: LispObject,
    noerror: LispObject,
    count: LispObject,
) -> LispObject {
    unsafe { search_command(regexp, bound, noerror, count, -1, 1, true) }
}

/// Search forward from point for regular expression REGEXP.
/// Find the longest match in accord with Posix regular expression rules.
/// Set point to the end of the occurrence found, and return point.
/// An optional second argument bounds the search; it is a buffer position.
///   The match found must not end after that position.  A value of nil
///   means search to the end of the accessible portion of the buffer.
/// Optional third argument, if t, means if fail just return nil (no error).
///   If not nil and not t, move to limit of search and return nil.
/// Optional fourth argument COUNT, if a positive number, means to search
///   for COUNT successive occurrences.  If COUNT is negative, search
///   backward, instead of forward, for -COUNT occurrences.  A value of
///   nil means the same as 1.
/// With COUNT positive, the match found is the COUNTth one (or first,
///   if COUNT is 1 or nil) in the buffer located entirely after the
///   origin of the search; correspondingly with COUNT negative.
///
/// Search case-sensitivity is determined by the value of the variable
/// `case-fold-search', which see.
///
/// See also the functions `match-beginning', `match-end', `match-string',
/// and `replace-match'.
#[lisp_fn(min = "1", intspec = "sPosix search: ")]
pub fn posix_search_forward(
    regexp: LispObject,
    bound: LispObject,
    noerror: LispObject,
    count: LispObject,
) -> LispObject {
    unsafe { search_command(regexp, bound, noerror, count, 1, 1, true) }
}

/// Return position of start of text matched by last search.
/// SUBEXP, a number, specifies which parenthesized expression in the last
///   regexp.
/// Value is nil if SUBEXPth pair didn't match, or there were less than
///   SUBEXP pairs.
/// Zero means the entire text matched by the whole regexp or whole string.
///
/// Return value is undefined if the last search failed.
#[lisp_fn]
pub fn match_beginning(subexp: LispObject) -> LispObject {
    unsafe { match_limit(subexp, true) }
}

/// Return position of end of text matched by last search.
/// SUBEXP, a number, specifies which parenthesized expression in the last
///   regexp.
/// Value is nil if SUBEXPth pair didn't match, or there were less than
///   SUBEXP pairs.
/// Zero means the entire text matched by the whole regexp or whole string.
///
/// Return value is undefined if the last search failed.
#[lisp_fn]
pub fn match_end(subexp: LispObject) -> LispObject {
    unsafe { match_limit(subexp, false) }
}

include!(concat!(env!("OUT_DIR"), "/search_exports.rs"));
