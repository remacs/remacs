
#[no_mangle]
pub extern "C" fn Ffontp(object: ::remacs_sys::Lisp_Object, extra_type: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = fontp(::lisp::LispObject::from_raw(object).into(), ::lisp::LispObject::from_raw(extra_type).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfontp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffontp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"fontp ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { fontp }