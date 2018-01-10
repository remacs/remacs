
#[no_mangle]
pub extern "C" fn Fmod(x: ::remacs_sys::Lisp_Object, y: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = lisp_mod(::lisp::LispObject::from_raw(x).into(), ::lisp::LispObject::from_raw(y).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smod: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmod as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"mod ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fplus(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = plus(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Splus: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fplus as *const ::libc::c_void,
	        min_args: 0,
	        max_args: -2,
	        symbol_name: (b"+ ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fminus(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = minus(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sminus: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fminus as *const ::libc::c_void,
	        min_args: 0,
	        max_args: -2,
	        symbol_name: (b"- ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ftimes(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = times(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Stimes: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ftimes as *const ::libc::c_void,
	        min_args: 0,
	        max_args: -2,
	        symbol_name: (b"* ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fquo(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = quo(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Squo: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fquo as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"/ ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flogand(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = logand(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slogand: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flogand as *const ::libc::c_void,
	        min_args: 0,
	        max_args: -2,
	        symbol_name: (b"logand ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flogior(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = logior(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slogior: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flogior as *const ::libc::c_void,
	        min_args: 0,
	        max_args: -2,
	        symbol_name: (b"logior ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flogxor(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = logxor(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slogxor: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flogxor as *const ::libc::c_void,
	        min_args: 0,
	        max_args: -2,
	        symbol_name: (b"logxor ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fmax(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = max(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smax: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmax as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"max ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fmin(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = min(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Smin: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fmin as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"min ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fabs(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = abs(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sabs: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fabs as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"abs ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Feqlsign(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = eqlsign(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Seqlsign: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Feqlsign as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"= ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flss(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = lss(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slss: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flss as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"< ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fgtr(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = gtr(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sgtr: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fgtr as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"> ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fleq(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = leq(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sleq: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fleq as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b"<= ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fgeq(nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {
let args = unsafe {
::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,
nargs as usize)
};

	let ret = geq(unsafe { ::std::mem::transmute(args) });
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sgeq: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fgeq as *const ::libc::c_void,
	        min_args: 1,
	        max_args: -2,
	        symbol_name: (b">= ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fneq(num1: ::remacs_sys::Lisp_Object, num2: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = neq(::lisp::LispObject::from_raw(num1).into(), ::lisp::LispObject::from_raw(num2).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sneq: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fneq as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"/= ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Frem(x: ::remacs_sys::Lisp_Object, y: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = rem(::lisp::LispObject::from_raw(x).into(), ::lisp::LispObject::from_raw(y).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Srem: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Frem as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"% ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fadd1(number: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = add1(::lisp::LispObject::from_raw(number).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sadd1: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fadd1 as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"1+ ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsub1(number: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = sub1(::lisp::LispObject::from_raw(number).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssub1: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsub1 as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"1- ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flognot(number: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = lognot(::lisp::LispObject::from_raw(number).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slognot: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flognot as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"lognot ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { mod, plus, minus, times, quo, logand, logior, logxor, max, min, abs, eqlsign, lss, gtr, leq, geq, neq, rem, add1, sub1, lognot }