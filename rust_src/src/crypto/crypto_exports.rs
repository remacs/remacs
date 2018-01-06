
#[no_mangle]
pub extern "C" fn Fmd5(object: ::remacs_sys::Lisp_Object, start: ::remacs_sys::Lisp_Object, end: ::remacs_sys::Lisp_Object, coding_system: ::remacs_sys::Lisp_Object, noerror: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = md5(::lisp::LispObject::from_raw(object).into(), ::lisp::LispObject::from_raw(start).into(), ::lisp::LispObject::from_raw(end).into(), ::lisp::LispObject::from_raw(coding_system).into(), ::lisp::LispObject::from_raw(noerror).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smd5: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmd5 as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 5,
	        symbol_name: (b"md5 ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsecure_hash(algorithm: ::remacs_sys::Lisp_Object, object: ::remacs_sys::Lisp_Object, start: ::remacs_sys::Lisp_Object, end: ::remacs_sys::Lisp_Object, binary: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = secure_hash(::lisp::LispObject::from_raw(algorithm).into(), ::lisp::LispObject::from_raw(object).into(), ::lisp::LispObject::from_raw(start).into(), ::lisp::LispObject::from_raw(end).into(), ::lisp::LispObject::from_raw(binary).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssecure_hash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsecure_hash as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 5,
	        symbol_name: (b"secure-hash ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fbuffer_hash(buffer_or_name: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = buffer_hash(::lisp::LispObject::from_raw(buffer_or_name).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sbuffer_hash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fbuffer_hash as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"buffer-hash ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { md5, secure_hash, buffer_hash }