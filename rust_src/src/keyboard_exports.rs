
#[no_mangle]
pub extern "C" fn Fposn_at_point(pos: ::remacs_sys::Lisp_Object, window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = posn_at_point(::lisp::LispObject::from_raw(pos).into(), ::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sposn_at_point: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fposn_at_point as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 2,
	        symbol_name: (b"posn-at-point ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fposn_at_x_y(objx: ::remacs_sys::Lisp_Object, objy: ::remacs_sys::Lisp_Object, frame_or_window: ::remacs_sys::Lisp_Object, whole: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = posn_at_x_y(::lisp::LispObject::from_raw(objx).into(), ::lisp::LispObject::from_raw(objy).into(), ::lisp::LispObject::from_raw(frame_or_window).into(), ::lisp::LispObject::from_raw(whole).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sposn_at_x_y: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fposn_at_x_y as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 4,
	        symbol_name: (b"posn-at-x-y ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { posn_at_point, posn_at_x_y }