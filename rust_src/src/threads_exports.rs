
#[no_mangle]
pub extern "C" fn Fthread_name(thread: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = thread_name(::lisp::LispObject::from_raw(thread).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sthread_name: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fthread_name as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"thread-name ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fthread_alive_p(thread: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = thread_alive_p(::lisp::LispObject::from_raw(thread).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sthread_alive_p: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fthread_alive_p as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"thread-alive-p ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { thread_name, thread_alive_p }