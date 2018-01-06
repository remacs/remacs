
#[no_mangle]
pub extern "C" fn Facos(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = acos(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sacos: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Facos as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"acos ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fasin(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = asin(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sasin: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fasin as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"asin ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fcos(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = cos(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Scos: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fcos as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"cos ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsin(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = sin(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssin: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsin as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"sin ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ftan(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = tan(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Stan: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ftan as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"tan ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fexp(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = exp(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sexp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fexp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"exp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fsqrt(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = sqrt(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Ssqrt: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fsqrt as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"sqrt ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fisnan(f: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = isnan(::lisp::LispObject::from_raw(f).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sisnan: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fisnan as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"isnan ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fatan(y: ::remacs_sys::Lisp_Object, x: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = atan(::lisp::LispObject::from_raw(y).into(), ::lisp::LispObject::from_raw(x).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Satan: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fatan as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"atan ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flog(arg: ::remacs_sys::Lisp_Object, base: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = log(::lisp::LispObject::from_raw(arg).into(), ::lisp::LispObject::from_raw(base).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slog: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flog as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"log ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffceiling(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = fceiling(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfceiling: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffceiling as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"fceiling ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fffloor(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = ffloor(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sffloor: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fffloor as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"ffloor ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fftruncate(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = ftruncate(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sftruncate: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fftruncate as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"ftruncate ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffloat(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = float(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfloat: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffloat as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"float ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fcopysign(x1: ::remacs_sys::Lisp_Object, x2: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = copysign(::lisp::LispObject::from_raw(x1).into(), ::lisp::LispObject::from_raw(x2).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Scopysign: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fcopysign as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"copysign ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffrexp(x: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = frexp(::lisp::LispObject::from_raw(x).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfrexp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffrexp as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"frexp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fldexp(significand: ::remacs_sys::Lisp_Object, exponent: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = ldexp(::lisp::LispObject::from_raw(significand).into(), ::lisp::LispObject::from_raw(exponent).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sldexp: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fldexp as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"ldexp ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fexpt(arg1: ::remacs_sys::Lisp_Object, arg2: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = expt(::lisp::LispObject::from_raw(arg1).into(), ::lisp::LispObject::from_raw(arg2).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sexpt: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fexpt as *const ::libc::c_void,
	        min_args: 2,
	        max_args: 2,
	        symbol_name: (b"expt ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Flogb(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = logb(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Slogb: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Flogb as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"logb ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffround(arg: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = fround(::lisp::LispObject::from_raw(arg).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfround: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffround as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 1,
	        symbol_name: (b"fround ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fceiling(arg: ::remacs_sys::Lisp_Object, divisor: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = ceiling(::lisp::LispObject::from_raw(arg).into(), ::lisp::LispObject::from_raw(divisor).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sceiling: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fceiling as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"ceiling ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ffloor(arg: ::remacs_sys::Lisp_Object, divisor: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = floor(::lisp::LispObject::from_raw(arg).into(), ::lisp::LispObject::from_raw(divisor).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sfloor: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ffloor as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"floor ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Fround(arg: ::remacs_sys::Lisp_Object, divisor: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = round(::lisp::LispObject::from_raw(arg).into(), ::lisp::LispObject::from_raw(divisor).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Sround: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Fround as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"round ").as_ptr() as *const ::libc::c_char,
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
pub extern "C" fn Ftruncate(arg: ::remacs_sys::Lisp_Object, divisor: ::remacs_sys::Lisp_Object) -> ::remacs_sys::Lisp_Object {

	let ret = truncate(::lisp::LispObject::from_raw(arg).into(), ::lisp::LispObject::from_raw(divisor).into());
	::lisp::LispObject::from(ret).to_raw()
}

lazy_static! {
	pub static ref Struncate: ::lisp::LispSubrRef = {
	    let subr = ::remacs_sys::Lisp_Subr {
	        header: ::remacs_sys::Lisp_Vectorlike_Header {
	            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
	                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) ,
	        },
	        function: self::Ftruncate as *const ::libc::c_void,
	        min_args: 1,
	        max_args: 2,
	        symbol_name: (b"truncate ").as_ptr() as *const ::libc::c_char,
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
export_lisp_fns! { acos, asin, cos, sin, tan, exp, sqrt, isnan, atan, log, fceiling, ffloor, ftruncate, float, copysign, frexp, ldexp, expt, logb, fround, ceiling, floor, round, truncate }