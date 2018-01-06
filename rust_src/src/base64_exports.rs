
#[no_mangle]
pub extern "C" fn Fbase64_encode_string(string: ::remacs_sys::Lisp_Object, no_line_break: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = base64_encode_string(::lisp::LispObject::from_raw(string).into(), ::lisp::LispObject::from_raw(no_line_break).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sbase64_encode_string: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fbase64_encode_string as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"base64-encode-string ").as_ptr() as *const ::libc::c_char,
	        intspec: ::std::ptr::null(),
	        doc: ::std::ptr::null(),
	        lang: ::remacs_sys::Lisp_Subr_Lang_Rust,
	    };

	    unsafe {
	        let ptr =
	            ::remacs_sys::xmalloc(::std::mem::size_of::<::remacs_sys::Lisp_Subr>())
	            as *mut ::remacs_sys::Lisp_Subr;
	        ::std::ptr::copy_nonoverlapping(&subr, ptr, 1);
	        ::std::mem::forget(subr);
	        ::lisp::ExternalPtr::new(ptr)
	    }
	};
}

#[no_mangle]
pub extern "C" fn Fbase64_decode_string(string: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = base64_decode_string(::lisp::LispObject::from_raw(string).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sbase64_decode_string: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fbase64_decode_string as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"base64-decode-string ").as_ptr() as *const ::libc::c_char,
	        intspec: ::std::ptr::null(),
	        doc: ::std::ptr::null(),
	        lang: ::remacs_sys::Lisp_Subr_Lang_Rust,
	    };

	    unsafe {
	        let ptr =
	            ::remacs_sys::xmalloc(::std::mem::size_of::<::remacs_sys::Lisp_Subr>())
	            as *mut ::remacs_sys::Lisp_Subr;
	        ::std::ptr::copy_nonoverlapping(&subr, ptr, 1);
	        ::std::mem::forget(subr);
	        ::lisp::ExternalPtr::new(ptr)
	    }
	};
}

// exports
export_lisp_fns! { base64_encode_string, base64_decode_string }