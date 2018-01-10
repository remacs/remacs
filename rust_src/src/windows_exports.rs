
#[no_mangle]
pub extern "C" fn Fwindowp(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = windowp(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindowp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindowp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"windowp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_live_p(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_live_p(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_live_p: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_live_p as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"window-live-p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_point(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_point(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_point: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_point as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-point ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fselected_window() -> ::remacs_sys::Lisp_Object {

	let ret = selected_window();
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sselected_window: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fselected_window as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 0,
	        symbol_name: (b"selected-window ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_buffer(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_buffer(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_buffer: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_buffer as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-buffer ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_valid_p(object: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_valid_p(::lisp::LispObject::from_raw(object).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_valid_p: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_valid_p as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"window-valid-p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_start(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_start(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_start: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_start as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-start ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_minibuffer_p(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_minibuffer_p(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_minibuffer_p: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_minibuffer_p as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-minibuffer-p ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_margins(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_margins(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_margins: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_margins as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-margins ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_combination_limit(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_combination_limit(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_combination_limit: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_combination_limit as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"window-combination-limit ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fset_window_combination_limit(window: ::remacs_sys::Lisp_Object, limit: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = set_window_combination_limit(::lisp::LispObject::from_raw(window).into(), ::lisp::LispObject::from_raw(limit).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sset_window_combination_limit: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fset_window_combination_limit as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"set-window-combination-limit ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fminibuffer_selected_window() -> ::remacs_sys::Lisp_Object {

	let ret = minibuffer_selected_window();
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sminibuffer_selected_window: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fminibuffer_selected_window as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 0,
	        symbol_name: (b"minibuffer-selected-window ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_total_width(window: ::remacs_sys::Lisp_Object, round: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_total_width(::lisp::LispObject::from_raw(window).into(), ::lisp::LispObject::from_raw(round).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_total_width: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_total_width as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 2,
	        symbol_name: (b"window-total-width ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_total_height(window: ::remacs_sys::Lisp_Object, round: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_total_height(::lisp::LispObject::from_raw(window).into(), ::lisp::LispObject::from_raw(round).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_total_height: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_total_height as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 2,
	        symbol_name: (b"window-total-height ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_parent(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_parent(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_parent: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_parent as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-parent ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fwindow_frame(window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = window_frame(::lisp::LispObject::from_raw(window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Swindow_frame: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fwindow_frame as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"window-frame ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fframe_root_window(frame_or_window: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = frame_root_window(::lisp::LispObject::from_raw(frame_or_window).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sframe_root_window: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fframe_root_window as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"frame-root-window ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fminibuffer_window(frame: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = minibuffer_window(::lisp::LispObject::from_raw(frame).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sminibuffer_window: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fminibuffer_window as *const ::libc::c_void,
	        min_args: 0,
	        max_args: 1,
	        symbol_name: (b"minibuffer-window ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { windowp, window_live_p, window_point, selected_window, window_buffer, window_valid_p, window_start, window_minibuffer_p, window_margins, window_combination_limit, set_window_combination_limit, minibuffer_selected_window, window_total_width, window_total_height, window_parent, window_frame, frame_root_window, minibuffer_window }