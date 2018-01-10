
#[no_mangle]
pub extern "C" fn Fprocessp(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = processp(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocessp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocessp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"processp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fget_process(name: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = get_process(::lisp::LispObject::from_raw(name).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sget_process: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fget_process as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"get-process ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_name(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_name(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_name: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_name as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-name ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_buffer(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_buffer(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_buffer: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_buffer as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-buffer ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_id(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_id(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_id: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_id as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-id ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fget_buffer_process(buffer: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = get_buffer_process(::lisp::LispObject::from_raw(buffer).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sget_buffer_process: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fget_buffer_process as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"get-buffer-process ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_tty_name(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_tty_name(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_tty_name: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_tty_name as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-tty-name ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_command(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_command(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_command: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_command as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-command ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_filter(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_filter(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_filter: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_filter as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-filter ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_sentinel(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_sentinel(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_sentinel: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_sentinel as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-sentinel ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_mark(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_mark(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_mark: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_mark as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-mark ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_list() -> ::remacs_sys::Lisp_Object {

	let ret = process_list();
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_list: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_list as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 0,
	        symbol_name: (b"process-list ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_plist(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_plist(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_plist: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_plist as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-plist ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fset_process_plist(process: ::remacs_sys::Lisp_Object, plist: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = set_process_plist(::lisp::LispObject::from_raw(process).into(), ::lisp::LispObject::from_raw(plist).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sset_process_plist: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fset_process_plist as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"set-process-plist ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_status(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_status(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_status: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_status as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-status ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fset_process_buffer(process: ::remacs_sys::Lisp_Object, buffer: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = set_process_buffer(::lisp::LispObject::from_raw(process).into(), ::lisp::LispObject::from_raw(buffer).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sset_process_buffer: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fset_process_buffer as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"set-process-buffer ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_send_string(process: ::remacs_sys::Lisp_Object, string: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_send_string(::lisp::LispObject::from_raw(process).into(), ::lisp::LispObject::from_raw(string).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_send_string: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_send_string as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"process-send-string ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fprocess_query_on_exit_flag(process: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = process_query_on_exit_flag(::lisp::LispObject::from_raw(process).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sprocess_query_on_exit_flag: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fprocess_query_on_exit_flag as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"process-query-on-exit-flag ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fset_process_query_on_exit_flag(process: ::remacs_sys::Lisp_Object, flag: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = set_process_query_on_exit_flag(::lisp::LispObject::from_raw(process).into(), ::lisp::LispObject::from_raw(flag).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sset_process_query_on_exit_flag: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fset_process_query_on_exit_flag as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"set-process-query-on-exit-flag ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { processp, get_process, process_name, process_buffer, process_id, get_buffer_process, process_tty_name, process_command, process_filter, process_sentinel, process_mark, process_list, process_plist, set_process_plist, process_status, set_process_buffer, process_send_string, process_query_on_exit_flag, set_process_query_on_exit_flag }