
#[no_mangle]
pub extern "C" fn Fintern_soft(name: ::remacs_sys::Lisp_Object, obarray: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = intern_soft(::lisp::LispObject::from_raw(name).into(), ::lisp::LispObject::from_raw(obarray).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sintern_soft: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fintern_soft as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"intern-soft ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fintern(string: ::remacs_sys::Lisp_Object, obarray: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = intern(::lisp::LispObject::from_raw(string).into(), ::lisp::LispObject::from_raw(obarray).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sintern: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fintern as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"intern ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { intern_soft, intern }