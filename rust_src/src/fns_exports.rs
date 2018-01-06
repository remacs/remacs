
#[no_mangle]
pub extern "C" fn Ffeaturep(feature: ::remacs_sys::Lisp_Object, subfeature: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = featurep(::lisp::LispObject::from_raw(feature).into(), ::lisp::LispObject::from_raw(subfeature).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfeaturep: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffeaturep as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"featurep ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprovide(feature: ::remacs_sys::Lisp_Object, subfeature: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = provide(::lisp::LispObject::from_raw(feature).into(), ::lisp::LispObject::from_raw(subfeature).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprovide: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprovide as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"provide ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fquote(args: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = quote(::lisp::LispObject::from_raw(args).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Squote: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fquote as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -1,
	        symbol_name: (b"quote ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { featurep, provide, quote }