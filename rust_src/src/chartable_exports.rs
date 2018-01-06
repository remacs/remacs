
#[no_mangle]
pub extern "C" fn Fchar_table_subtype(chartable: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = char_table_subtype(::lisp::LispObject::from_raw(chartable).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Schar_table_subtype: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fchar_table_subtype as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"char-table-subtype ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fchar_table_parent(chartable: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = char_table_parent(::lisp::LispObject::from_raw(chartable).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Schar_table_parent: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fchar_table_parent as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"char-table-parent ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fset_char_table_parent(chartable: ::remacs_sys::Lisp_Object, parent: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = set_char_table_parent(::lisp::LispObject::from_raw(chartable).into(), ::lisp::LispObject::from_raw(parent).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sset_char_table_parent: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fset_char_table_parent as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"set-char-table-parent ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { char_table_subtype, char_table_parent, set_char_table_parent }