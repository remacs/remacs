
#[no_mangle]
pub extern "C" fn Flocal_key_binding(keys: ::remacs_sys::Lisp_Object, accept_default: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = local_key_binding(::lisp::LispObject::from_raw(keys).into(), ::lisp::LispObject::from_raw(accept_default).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slocal_key_binding: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flocal_key_binding as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"local-key-binding ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fcurrent_local_map() -> ::remacs_sys::Lisp_Object {

	let ret = current_local_map();
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Scurrent_local_map: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fcurrent_local_map as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 0,
	        symbol_name: (b"current-local-map ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fcurrent_global_map() -> ::remacs_sys::Lisp_Object {

	let ret = current_global_map();
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Scurrent_global_map: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fcurrent_global_map as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 0,
	        symbol_name: (b"current-global-map ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flookup_key(keymap: ::remacs_sys::Lisp_Object, key: ::remacs_sys::Lisp_Object, accept_default: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = lookup_key(::lisp::LispObject::from_raw(keymap).into(), ::lisp::LispObject::from_raw(key).into(), ::lisp::LispObject::from_raw(accept_default).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slookup_key: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flookup_key as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 3,
	        symbol_name: (b"lookup-key ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fdefine_prefix_command(command: ::remacs_sys::Lisp_Object, mapvar: ::remacs_sys::Lisp_Object, name: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = define_prefix_command(::lisp::LispObject::from_raw(command).into(), ::lisp::LispObject::from_raw(mapvar).into(), ::lisp::LispObject::from_raw(name).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sdefine_prefix_command: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fdefine_prefix_command as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 3,
	        symbol_name: (b"define-prefix-command ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fmake_sparse_keymap(string: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = make_sparse_keymap(::lisp::LispObject::from_raw(string).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smake_sparse_keymap: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmake_sparse_keymap as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"make-sparse-keymap ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { local_key_binding, current_local_map, current_global_map, lookup_key, define_prefix_command, make_sparse_keymap }