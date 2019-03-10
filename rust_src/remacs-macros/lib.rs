#![recursion_limit = "128"]

#[macro_use]
extern crate lazy_static;
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate regex;
extern crate remacs_util;
extern crate syn;

use proc_macro::TokenStream;
use regex::Regex;

mod function;

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let fn_item = syn::parse(fn_ts.clone()).unwrap();
    let function = function::parse(&fn_item).unwrap();
    let lisp_fn_args = match remacs_util::parse_lisp_fn(
        &attr_ts.to_string(),
        &function.name,
        function.fntype.def_min_args(),
    ) {
        Ok(v) => v,
        Err(e) => panic!("Invalid lisp_fn attribute: {}", e),
    };

    let mut cargs = quote::Tokens::new();
    let mut rargs = quote::Tokens::new();
    let mut body = quote::Tokens::new();
    let max_args = function.args.len() as i16;
    let intspec = if let Some(intspec) = lisp_fn_args.intspec {
        let cbyte_intspec = CByteLiteral(intspec.as_str());
        quote! { (#cbyte_intspec).as_ptr() as *const libc::c_char }
    } else {
        quote! { std::ptr::null() }
    };

    match function.fntype {
        function::LispFnType::Normal(_) => {
            for ident in function.args {
                let arg = quote! { #ident: crate::lisp::LispObject, };
                cargs.append_all(arg);

                let arg = quote! { (#ident).into(), };
                rargs.append_all(arg);
            }
        }
        function::LispFnType::Many => {
            let args = quote! {
                nargs: libc::ptrdiff_t,
                args: *mut crate::lisp::LispObject,
            };
            cargs.append_all(args);

            let b = quote! {
                let args = unsafe {
                    std::slice::from_raw_parts_mut::<crate::lisp::LispObject>(
                        args, nargs as usize)
                };
            };
            body.append_all(b);

            let arg = quote! { unsafe { std::mem::transmute(args) } };
            rargs.append_all(arg);
        }
    }

    let cname = lisp_fn_args.c_name;
    let sname = concat_idents("S", &cname);
    let fname = concat_idents("F", &cname);
    let rname = function.name;
    let min_args = lisp_fn_args.min;
    let mut windows_header = quote! {};

    let functype = if lisp_fn_args.unevalled {
        quote! { aUNEVALLED }
    } else {
        match function.fntype {
            function::LispFnType::Normal(_) => match max_args {
                0 => quote! { a0 },
                1 => quote! { a1 },
                2 => quote! { a2 },
                3 => quote! { a3 },
                4 => quote! { a4 },
                5 => quote! { a5 },
                6 => quote! { a6 },
                7 => quote! { a7 },
                8 => quote! { a8 },
                _ => panic!("max_args too high"),
            },
            function::LispFnType::Many => quote! { aMANY },
        }
    };

    let max_args = if lisp_fn_args.unevalled {
        quote! { -1 }
    } else {
        match function.fntype {
            function::LispFnType::Normal(_) => quote! { #max_args },
            function::LispFnType::Many => quote! { crate::lisp::MANY  },
        }
    };
    let symbol_name = CByteLiteral(&lisp_fn_args.name);

    if cfg!(windows) {
        windows_header = quote! {
            | (std::mem::size_of::<crate::remacs_sys::Lisp_Subr>()
               / std::mem::size_of::<crate::remacs_sys::EmacsInt>()) as libc::ptrdiff_t
        };
    }

    let tokens = quote! {
        #[no_mangle]
        #[allow(clippy::not_unsafe_ptr_arg_deref)]
        #[allow(clippy::transmute_ptr_to_ptr)]
        #[allow(clippy::diverging_sub_expression)]
        pub extern "C" fn #fname(#cargs) -> crate::lisp::LispObject {
            #body

            let ret = #rname(#rargs);
            #[allow(unreachable_code)]
            crate::lisp::LispObject::from(ret)
        }

        lazy_static! {
            pub static ref #sname: crate::lisp::LispSubrRef = {
                let subr = crate::remacs_sys::Lisp_Subr {
                    header: crate::remacs_sys::vectorlike_header {
                        size: ((crate::remacs_sys::pvec_type::PVEC_SUBR as libc::ptrdiff_t)
                               << crate::remacs_sys::More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS)
                            #windows_header,
                    },
                    function: crate::remacs_sys::Lisp_Subr__bindgen_ty_1 {
                        #functype: (Some(self::#fname))
                    },
                    min_args: #min_args,
                    max_args: #max_args,
                    symbol_name: (#symbol_name).as_ptr() as *const libc::c_char,
                    intspec: #intspec,
                    doc: 0,
                    lang: crate::remacs_sys::Lisp_Subr_Lang::Lisp_Subr_Lang_Rust,
                };

                unsafe {
                    let ptr =
                        crate::remacs_sys::xmalloc(
                            std::mem::size_of::<crate::remacs_sys::Lisp_Subr>()
                        ) as *mut crate::remacs_sys::Lisp_Subr;
                    std::ptr::copy_nonoverlapping(&subr, ptr, 1);
                    crate::lisp::ExternalPtr::new(ptr)
                }
            };
        }
    };

    // we could put #fn_item into the quoted code above, but doing so
    // drops all of the line numbers on the floor and causes the
    // compiler to attribute any errors in the function to the macro
    // invocation instead.
    let tokens: TokenStream = tokens.into();
    tokens.into_iter().chain(fn_ts.into_iter()).collect()
}

struct CByteLiteral<'a>(&'a str);

impl<'a> quote::ToTokens for CByteLiteral<'a> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r#"["\\]"#).unwrap();
        }
        let s = RE.replace_all(self.0, |caps: &regex::Captures| {
            format!("\\x{:x}", u32::from(caps[0].chars().next().unwrap()))
        });
        tokens.append_all(
            (syn::parse_str::<syn::Expr>(&format!(r#"b"{}\0""#, s)))
                .unwrap()
                .into_tokens(),
        );
    }
}

fn concat_idents(lhs: &str, rhs: &str) -> syn::Ident {
    syn::Ident::from(format!("{}{}", lhs, rhs))
}
