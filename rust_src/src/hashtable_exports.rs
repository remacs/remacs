
#[no_mangle]
pub extern "C" fn Fcopy_hash_table(table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = copy_hash_table(::lisp::LispObject::from_raw(table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Scopy_hash_table: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fcopy_hash_table as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"copy-hash-table ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fgethash(key: ::remacs_sys::Lisp_Object, hash_table: ::remacs_sys::Lisp_Object, dflt: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = gethash(::lisp::LispObject::from_raw(key).into(), ::lisp::LispObject::from_raw(hash_table).into(), ::lisp::LispObject::from_raw(dflt).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sgethash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fgethash as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 3,
	        symbol_name: (b"gethash ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fputhash(key: ::remacs_sys::Lisp_Object, value: ::remacs_sys::Lisp_Object, hash_table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = puthash(::lisp::LispObject::from_raw(key).into(), ::lisp::LispObject::from_raw(value).into(), ::lisp::LispObject::from_raw(hash_table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sputhash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fputhash as *const ::libc::c_void,
	        min_args: 3,
	        max_args: 3,
	        symbol_name: (b"puthash ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fremhash(key: ::remacs_sys::Lisp_Object, hash_table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = remhash(::lisp::LispObject::from_raw(key).into(), ::lisp::LispObject::from_raw(hash_table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sremhash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fremhash as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"remhash ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fmaphash(function: ::remacs_sys::Lisp_Object, hash_table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = maphash(::lisp::LispObject::from_raw(function).into(), ::lisp::LispObject::from_raw(hash_table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smaphash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmaphash as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"maphash ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fhash_table_p(obj: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = hash_table_p(::lisp::LispObject::from_raw(obj).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Shash_table_p: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fhash_table_p as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"hash-table-p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fhash_table_count(table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = hash_table_count(::lisp::LispObject::from_raw(table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Shash_table_count: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fhash_table_count as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"hash-table-count ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fhash_table_rehash_threshold(table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = hash_table_rehash_threshold(::lisp::LispObject::from_raw(table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Shash_table_rehash_threshold: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fhash_table_rehash_threshold as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"hash-table-rehash-threshold ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fhash_table_size(table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = hash_table_size(::lisp::LispObject::from_raw(table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Shash_table_size: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fhash_table_size as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"hash-table-size ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fhash_table_test(table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = hash_table_test(::lisp::LispObject::from_raw(table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Shash_table_test: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fhash_table_test as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"hash-table-test ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fhash_table_weakness(table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = hash_table_weakness(::lisp::LispObject::from_raw(table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Shash_table_weakness: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fhash_table_weakness as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"hash-table-weakness ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fclrhash(hash_table: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = clrhash(::lisp::LispObject::from_raw(hash_table).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sclrhash: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fclrhash as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"clrhash ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fdefine_hash_table_test(name: ::remacs_sys::Lisp_Object, test: ::remacs_sys::Lisp_Object, hash: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = define_hash_table_test(::lisp::LispObject::from_raw(name).into(), ::lisp::LispObject::from_raw(test).into(), ::lisp::LispObject::from_raw(hash).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sdefine_hash_table_test: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fdefine_hash_table_test as *const ::libc::c_void,
	        min_args: 3,
	        max_args: 3,
	        symbol_name: (b"define-hash-table-test ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { copy_hash_table, gethash, puthash, remhash, maphash, hash_table_p, hash_table_count, hash_table_rehash_threshold, hash_table_size, hash_table_test, hash_table_weakness, clrhash, define_hash_table_test }