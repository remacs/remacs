
#[no_mangle]
pub extern "C" fn Fsymbolp(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = symbolp(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssymbolp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsymbolp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"symbolp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsymbol_name(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = symbol_name(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssymbol_name: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsymbol_name as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"symbol-name ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffboundp(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = fboundp(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfboundp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffboundp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"fboundp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsymbol_function(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = symbol_function(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssymbol_function: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsymbol_function as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"symbol-function ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsymbol_plist(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = symbol_plist(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssymbol_plist: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsymbol_plist as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"symbol-plist ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsetplist(symbol: ::remacs_sys::Lisp_Object, newplist: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = setplist(::lisp::LispObject::from_raw(symbol).into(), ::lisp::LispObject::from_raw(newplist).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssetplist: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsetplist as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"setplist ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffmakunbound(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = fmakunbound(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfmakunbound: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffmakunbound as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"fmakunbound ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fkeywordp(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = keywordp(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Skeywordp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fkeywordp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"keywordp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Findirect_variable(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = indirect_variable_lisp(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sindirect_variable: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Findirect_variable as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"indirect-variable ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fmakunbound(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = makunbound(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smakunbound: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmakunbound as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"makunbound ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsymbol_value(symbol: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = symbol_value(::lisp::LispObject::from_raw(symbol).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssymbol_value: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsymbol_value as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"symbol-value ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { symbolp, symbol_name, fboundp, symbol_function, symbol_plist, setplist, fmakunbound, keywordp, indirect_variable, makunbound, symbol_value }