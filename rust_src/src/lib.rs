#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]
#![feature(proc_macro)]
#![cfg_attr(feature = "strict", deny(warnings))]
#![feature(global_allocator)]

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test), target_os = "macos"))]
extern crate alloc_unexecmacosx;

#[macro_use]
extern crate lazy_static;

extern crate remacs_sys;

// Needed for linking.
extern crate remacs_lib;

extern crate base64 as base64_crate;
extern crate libc;
extern crate md5;
extern crate rand;
extern crate remacs_macros;
extern crate sha1;
extern crate sha2;

#[cfg(test)]
extern crate mock_derive;

#[cfg(test)]
mod functions;

#[macro_use]
mod eval;
mod lisp;
mod lists;
mod marker;
mod floatfns;
mod math;
mod numbers;
mod objects;
mod strings;
mod symbols;
#[macro_use]
mod vectors;
mod character;
mod base64;
mod crypto;
mod str2sig;
mod multibyte;
mod buffers;
mod windows;
mod frames;
mod hashtable;
mod interactive;
mod process;
mod fonts;
mod threads;
mod chartable;
mod category;
mod obarray;
mod editfns;
mod util;
mod minibuf;
mod cmds;
mod data;
mod fns;
mod dispnew;
mod indent;

#[cfg(all(not(test), target_os = "macos"))]
use alloc_unexecmacosx::OsxUnexecAlloc;

#[cfg(all(not(test), target_os = "macos"))]
#[global_allocator]
static ALLOCATOR: OsxUnexecAlloc = OsxUnexecAlloc;

pub use base64::base64_encode_1;
pub use base64::base64_decode_1;

pub use util::clip_to_bounds;

// Used in buffer.c
pub use buffers::Fbuffer_live_p;
pub use buffers::Fbuffer_modified_p;

// Used in window.c
pub use windows::Fwindow_buffer;

// used in process.c
pub use buffers::Fbuffer_name;

pub use buffers::validate_region;

// Used in nsfns.m
pub use buffers::Fbuffer_file_name;

// These need to be exported as bytecode.c depends upon them.
pub use math::Fplus;
pub use math::Fminus;
pub use math::Ftimes;
pub use math::Fmax;
pub use math::Fmin;
pub use math::Fquo;
pub use math::Flss;
pub use math::Fleq;
pub use math::Frem;
pub use math::Fadd1;
pub use math::Fsub1;
pub use math::arithcompare;
pub use editfns::Feobp;
pub use editfns::Fbobp;
pub use editfns::Ffollowing_char;

// Widely used in the C codebase.
pub use lists::Fsetcar;
pub use lists::Fsetcdr;
pub use lists::Fcar;
pub use lists::Fcdr;
pub use lists::Fcar_safe;
pub use lists::Fcdr_safe;
pub use lists::Flistp;
pub use lists::Fnthcdr;
pub use lists::Fnth;
pub use lists::Fmemq;
pub use lists::Fmember;
pub use lists::Fassq;
pub use lists::Fassoc;
pub use lists::Frassq;
pub use lists::Frassoc;
pub use lists::Fdelq;
pub use lists::Fplist_get;
pub use lists::Fplist_member;
pub use lists::Fplist_put;
pub use lists::Fget;
pub use lists::Fput;
pub use lists::Flist;
pub use lists::Fmake_list;
pub use floatfns::extract_float;
pub use numbers::Frandom;
pub use objects::Fequal;
pub use objects::Fequal_including_properties;
pub use symbols::Fsymbolp;
pub use symbols::Fsymbol_name;
pub use symbols::Ffboundp;
pub use symbols::Fsymbol_function;
pub use symbols::Fsymbol_plist;
pub use symbols::Fkeywordp;
pub use symbols::Findirect_variable;
pub use symbols::indirect_variable;
pub use symbols::Fsymbol_value;
pub use strings::Fstring_equal;
pub use strings::Fstring_as_multibyte;
pub use strings::Fstring_to_multibyte;
pub use strings::Fstring_to_unibyte;
pub use strings::Fmultibyte_string_p;
pub use strings::Fstring_lessp;
pub use vectors::Flength;
pub use vectors::Felt;
pub use vectors::Fsort;
pub use lists::merge;
pub use buffers::Fget_buffer;
pub use buffers::Fcurrent_buffer;
pub use buffers::Fset_buffer;
pub use obarray::intern_1;
pub use obarray::Fintern;
pub use obarray::Fintern_soft;
pub use marker::Fmarker_position;
pub use marker::Fmarker_buffer;
pub use windows::Fwindow_point;
pub use editfns::Fgoto_char;
pub use data::Findirect_function;
pub use data::indirect_function;
pub use process::Fget_buffer_process;
pub use dispnew::Fsleep_for;

// Used in fileio.c
pub use editfns::Fpoint;

// used in chartab.c
pub use chartable::Fset_char_table_parent;

// used in category.c
pub use category::Fcategory_table_p;

// Used in process.c
pub use str2sig::str2sig;
pub use process::Fget_process;

// Used in character.c
pub use multibyte::char_resolve_modifier_mask;
pub use multibyte::char_string;
pub use multibyte::string_char;
pub use multibyte::count_size_as_multibyte;
pub use multibyte::multibyte_chars_in_text;
pub use multibyte::parse_str_as_multibyte;
pub use multibyte::str_as_multibyte;
pub use multibyte::str_to_multibyte;
pub use multibyte::str_as_unibyte;
pub use multibyte::str_to_unibyte;

// Used in bytecode.c, charset.c
pub use editfns::Fchar_after;

// Used in xdisp.c
pub use buffers::Foverlay_start;
pub use buffers::Foverlay_end;

// Used in window.c, macros.c
pub use interactive::Fprefix_numeric_value;
pub use editfns::Fbolp;
pub use editfns::Feolp;

// Used in minibuffer.c
pub use windows::Fwindow_minibuffer_p;

// Used in term.c, dired.c
pub use objects::Fidentity;

// Used in xdisp.c, coding.c, et. al.
pub use hashtable::Fgethash;
pub use hashtable::Fremhash;
pub use hashtable::Fputhash;
pub use hashtable::Fhash_table_rehash_threshold;

use lisp::defsubr;
use frames::rust_init_frame_syms;

#[cfg(test)]
pub use functions::make_float;

#[no_mangle]
pub extern "C" fn rust_init_syms() {
    unsafe {
        defsubr(buffers::Soverlayp.as_ptr());
        defsubr(buffers::Sbuffer_live_p.as_ptr());
        defsubr(buffers::Sget_buffer.as_ptr());
        defsubr(buffers::Scurrent_buffer.as_ptr());
        defsubr(buffers::Sbuffer_file_name.as_ptr());
        defsubr(buffers::Sbuffer_modified_p.as_ptr());
        defsubr(buffers::Sbuffer_modified_tick.as_ptr());
        defsubr(buffers::Sbuffer_chars_modified_tick.as_ptr());
        defsubr(buffers::Sbuffer_name.as_ptr());
        defsubr(buffers::Sset_buffer.as_ptr());
        defsubr(buffers::Soverlay_start.as_ptr());
        defsubr(buffers::Soverlay_end.as_ptr());
        defsubr(buffers::Soverlay_buffer.as_ptr());
        defsubr(windows::Swindowp.as_ptr());
        defsubr(windows::Swindow_minibuffer_p.as_ptr());
        defsubr(windows::Swindow_live_p.as_ptr());
        defsubr(windows::Swindow_point.as_ptr());
        defsubr(windows::Sselected_window.as_ptr());
        defsubr(windows::Swindow_buffer.as_ptr());
        defsubr(windows::Swindow_valid_p.as_ptr());
        defsubr(windows::Swindow_start.as_ptr());
        defsubr(windows::Swindow_margins.as_ptr());
        defsubr(windows::Swindow_combination_limit.as_ptr());
        defsubr(windows::Sset_window_combination_limit.as_ptr());
        defsubr(windows::Sminibuffer_selected_window.as_ptr());
        defsubr(windows::Swindow_total_height.as_ptr());
        defsubr(windows::Swindow_total_width.as_ptr());
        defsubr(windows::Swindow_frame.as_ptr());
        defsubr(process::Sget_process.as_ptr());
        defsubr(process::Sprocessp.as_ptr());
        defsubr(process::Sprocess_name.as_ptr());
        defsubr(process::Sprocess_buffer.as_ptr());
        defsubr(process::Sget_buffer_process.as_ptr());
        defsubr(lists::Satom.as_ptr());
        defsubr(lists::Slistp.as_ptr());
        defsubr(lists::Snlistp.as_ptr());
        defsubr(math::Smod.as_ptr());
        defsubr(math::Splus.as_ptr());
        defsubr(math::Sminus.as_ptr());
        defsubr(math::Stimes.as_ptr());
        defsubr(math::Squo.as_ptr());
        defsubr(math::Slogand.as_ptr());
        defsubr(math::Slogior.as_ptr());
        defsubr(math::Slogxor.as_ptr());
        defsubr(math::Smax.as_ptr());
        defsubr(math::Smin.as_ptr());
        defsubr(math::Sabs.as_ptr());
        defsubr(math::Seqlsign.as_ptr());
        defsubr(math::Slss.as_ptr());
        defsubr(math::Sgtr.as_ptr());
        defsubr(math::Sleq.as_ptr());
        defsubr(math::Sgeq.as_ptr());
        defsubr(math::Sneq.as_ptr());
        defsubr(math::Srem.as_ptr());
        defsubr(math::Sadd1.as_ptr());
        defsubr(math::Ssub1.as_ptr());
        defsubr(math::Slognot.as_ptr());
        defsubr(numbers::Sintegerp.as_ptr());
        defsubr(numbers::Sinteger_or_marker_p.as_ptr());
        defsubr(numbers::Sfloatp.as_ptr());
        defsubr(numbers::Snatnump.as_ptr());
        defsubr(numbers::Snumber_or_marker_p.as_ptr());
        defsubr(numbers::Snumberp.as_ptr());
        defsubr(numbers::Srandom.as_ptr());
        defsubr(objects::Snull.as_ptr());
        defsubr(objects::Seq.as_ptr());
        defsubr(objects::Seql.as_ptr());
        defsubr(objects::Sequal.as_ptr());
        defsubr(objects::Sequal_including_properties.as_ptr());
        defsubr(objects::Sidentity.as_ptr());
        defsubr(symbols::Ssymbolp.as_ptr());
        defsubr(symbols::Ssymbol_name.as_ptr());
        defsubr(symbols::Sfboundp.as_ptr());
        defsubr(symbols::Smakunbound.as_ptr());
        defsubr(symbols::Ssymbol_function.as_ptr());
        defsubr(symbols::Ssymbol_plist.as_ptr());
        defsubr(symbols::Ssetplist.as_ptr());
        defsubr(symbols::Sfmakunbound.as_ptr());
        defsubr(symbols::Skeywordp.as_ptr());
        defsubr(symbols::Sindirect_variable.as_ptr());
        defsubr(symbols::Ssymbol_value.as_ptr());
        defsubr(lists::Sconsp.as_ptr());
        defsubr(lists::Ssetcar.as_ptr());
        defsubr(lists::Ssetcdr.as_ptr());
        defsubr(lists::Scar.as_ptr());
        defsubr(lists::Scdr.as_ptr());
        defsubr(lists::Scar_safe.as_ptr());
        defsubr(lists::Scdr_safe.as_ptr());
        defsubr(lists::Snthcdr.as_ptr());
        defsubr(lists::Snth.as_ptr());
        defsubr(lists::Smemq.as_ptr());
        defsubr(lists::Smemql.as_ptr());
        defsubr(lists::Smember.as_ptr());
        defsubr(lists::Sassq.as_ptr());
        defsubr(lists::Sassoc.as_ptr());
        defsubr(lists::Srassq.as_ptr());
        defsubr(lists::Srassoc.as_ptr());
        defsubr(lists::Sdelq.as_ptr());
        defsubr(lists::Splist_get.as_ptr());
        defsubr(lists::Slax_plist_get.as_ptr());
        defsubr(lists::Splist_member.as_ptr());
        defsubr(lists::Splist_put.as_ptr());
        defsubr(lists::Slax_plist_put.as_ptr());
        defsubr(lists::Sget.as_ptr());
        defsubr(lists::Sput.as_ptr());
        defsubr(lists::Slist.as_ptr());
        defsubr(lists::Smake_list.as_ptr());
        defsubr(lists::Ssafe_length.as_ptr());
        defsubr(marker::Smarkerp.as_ptr());
        defsubr(marker::Smarker_position.as_ptr());
        defsubr(marker::Smarker_buffer.as_ptr());
        defsubr(strings::Sstringp.as_ptr());
        defsubr(strings::Smultibyte_string_p.as_ptr());
        defsubr(base64::Sbase64_encode_string.as_ptr());
        defsubr(base64::Sbase64_decode_string.as_ptr());
        defsubr(strings::Sstring_bytes.as_ptr());
        defsubr(strings::Sstring_equal.as_ptr());
        defsubr(strings::Sstring_as_multibyte.as_ptr());
        defsubr(strings::Sstring_to_multibyte.as_ptr());
        defsubr(strings::Sstring_to_unibyte.as_ptr());
        defsubr(strings::Sstring_lessp.as_ptr());
        defsubr(character::Smax_char.as_ptr());
        defsubr(character::Scharacterp.as_ptr());
        defsubr(character::Schar_or_string_p.as_ptr());
        defsubr(character::Sunibyte_char_to_multibyte.as_ptr());
        defsubr(character::Smultibyte_char_to_unibyte.as_ptr());
        defsubr(vectors::Sarrayp.as_ptr());
        defsubr(vectors::Sbool_vector_p.as_ptr());
        defsubr(vectors::Sbufferp.as_ptr());
        defsubr(vectors::Sbyte_code_function_p.as_ptr());
        defsubr(vectors::Schar_table_p.as_ptr());
        defsubr(vectors::Scondition_variable_p.as_ptr());
        defsubr(vectors::Smutexp.as_ptr());
        defsubr(vectors::Ssequencep.as_ptr());
        defsubr(vectors::Ssort.as_ptr());
        defsubr(vectors::Ssubrp.as_ptr());
        defsubr(vectors::Sthreadp.as_ptr());
        defsubr(vectors::Svector_or_char_table_p.as_ptr());
        defsubr(vectors::Svectorp.as_ptr());
        defsubr(vectors::Slength.as_ptr());
        defsubr(vectors::Selt.as_ptr());
        defsubr(vectors::Srecordp.as_ptr());
        defsubr(hashtable::Scopy_hash_table.as_ptr());
        defsubr(hashtable::Sgethash.as_ptr());
        defsubr(hashtable::Sremhash.as_ptr());
        defsubr(hashtable::Shash_table_p.as_ptr());
        defsubr(hashtable::Sputhash.as_ptr());
        defsubr(hashtable::Smaphash.as_ptr());
        defsubr(hashtable::Shash_table_count.as_ptr());
        defsubr(hashtable::Shash_table_rehash_threshold.as_ptr());
        defsubr(hashtable::Shash_table_size.as_ptr());
        defsubr(hashtable::Shash_table_test.as_ptr());
        defsubr(hashtable::Shash_table_weakness.as_ptr());
        defsubr(hashtable::Sclrhash.as_ptr());
        defsubr(hashtable::Sdefine_hash_table_test.as_ptr());
        defsubr(fonts::Sfontp.as_ptr());
        defsubr(crypto::Smd5.as_ptr());
        defsubr(crypto::Ssecure_hash.as_ptr());
        defsubr(crypto::Sbuffer_hash.as_ptr());
        defsubr(interactive::Sprefix_numeric_value.as_ptr());
        defsubr(chartable::Schar_table_subtype.as_ptr());
        defsubr(chartable::Schar_table_parent.as_ptr());
        defsubr(chartable::Sset_char_table_parent.as_ptr());
        defsubr(category::Scategory_table_p.as_ptr());
        defsubr(category::Scategory_table.as_ptr());
        defsubr(obarray::Sintern_soft.as_ptr());
        defsubr(obarray::Sintern.as_ptr());

        defsubr(floatfns::Sisnan.as_ptr());
        defsubr(floatfns::Sacos.as_ptr());
        defsubr(floatfns::Sasin.as_ptr());
        defsubr(floatfns::Satan.as_ptr());
        defsubr(floatfns::Scos.as_ptr());
        defsubr(floatfns::Ssin.as_ptr());
        defsubr(floatfns::Stan.as_ptr());
        defsubr(floatfns::Slog.as_ptr());
        defsubr(floatfns::Ssqrt.as_ptr());
        defsubr(floatfns::Sexp.as_ptr());
        defsubr(floatfns::Sffloor.as_ptr());
        defsubr(floatfns::Sfceiling.as_ptr());
        defsubr(floatfns::Sftruncate.as_ptr());
        defsubr(floatfns::Sfloat.as_ptr());
        defsubr(floatfns::Scopysign.as_ptr());
        defsubr(floatfns::Sfrexp.as_ptr());
        defsubr(floatfns::Sldexp.as_ptr());
        defsubr(floatfns::Sexpt.as_ptr());
        defsubr(floatfns::Slogb.as_ptr());
        defsubr(floatfns::Sfround.as_ptr());
        defsubr(floatfns::Sceiling.as_ptr());
        defsubr(floatfns::Sfloor.as_ptr());
        defsubr(floatfns::Sround.as_ptr());
        defsubr(floatfns::Struncate.as_ptr());
        defsubr(editfns::Spoint.as_ptr());
        defsubr(editfns::Sbuffer_size.as_ptr());
        defsubr(editfns::Seobp.as_ptr());
        defsubr(editfns::Sbobp.as_ptr());
        defsubr(editfns::Sbolp.as_ptr());
        defsubr(editfns::Seolp.as_ptr());
        defsubr(editfns::Sregion_beginning.as_ptr());
        defsubr(editfns::Sregion_end.as_ptr());
        defsubr(editfns::Smark_marker.as_ptr());
        defsubr(editfns::Spoint_min.as_ptr());
        defsubr(editfns::Spoint_max.as_ptr());
        defsubr(editfns::Sgoto_char.as_ptr());
        defsubr(editfns::Sposition_bytes.as_ptr());
        defsubr(editfns::Sinsert_byte.as_ptr());
        defsubr(editfns::Sfollowing_char.as_ptr());
        defsubr(editfns::Schar_after.as_ptr());
        defsubr(editfns::Spropertize.as_ptr());
        defsubr(fns::Sfeaturep.as_ptr());
        defsubr(minibuf::Sminibufferp.as_ptr());
        defsubr(minibuf::Sactive_minibuffer_window.as_ptr());
        defsubr(minibuf::Sset_minibuffer_window.as_ptr());
        defsubr(threads::Sthread_name.as_ptr());
        defsubr(cmds::Sforward_point.as_ptr());
        defsubr(data::Sindirect_function.as_ptr());
        defsubr(dispnew::Ssleep_for.as_ptr());
        defsubr(indent::Scurrent_column.as_ptr());
        defsubr(process::Sprocess_list.as_ptr());
        defsubr(process::Sset_process_plist.as_ptr());
    }
    rust_init_frame_syms();
}
