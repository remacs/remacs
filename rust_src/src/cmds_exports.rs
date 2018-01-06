
#[no_mangle]
pub extern "C" fn Fforward_char(n: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = forward_char(::lisp::LispObject::from_raw(n).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sforward_char: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fforward_char as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"forward-char ").as_ptr() as *const ::libc::c_char,
	        intspec: (b"^p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fbackward_char(n: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = backward_char(::lisp::LispObject::from_raw(n).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sbackward_char: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fbackward_char as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"backward-char ").as_ptr() as *const ::libc::c_char,
	        intspec: (b"^p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fforward_point(n: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = forward_point(::lisp::LispObject::from_raw(n).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sforward_point: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fforward_point as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"forward-point ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fbeginning_of_line(n: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = beginning_of_line(::lisp::LispObject::from_raw(n).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sbeginning_of_line: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fbeginning_of_line as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"beginning-of-line ").as_ptr() as *const ::libc::c_char,
	        intspec: (b"^p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fend_of_line(n: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = end_of_line(::lisp::LispObject::from_raw(n).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Send_of_line: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fend_of_line as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"end-of-line ").as_ptr() as *const ::libc::c_char,
	        intspec: (b"^p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fforward_line(n: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = forward_line(::lisp::LispObject::from_raw(n).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sforward_line: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fforward_line as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"forward-line ").as_ptr() as *const ::libc::c_char,
	        intspec: (b"^p ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { forward_char, backward_char, forward_point, beginning_of_line, end_of_line, forward_line }