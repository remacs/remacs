#![feature(proc_macro)]
#![recursion_limit = "128"]

#[macro_use]
extern crate synom;
extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;

use proc_macro::TokenStream;
use std::str::FromStr;

mod lisp_attr;
mod function;

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let fn_item = syn::parse_item(&fn_ts.to_string()).unwrap();
    let function = function::parse(&fn_item).unwrap();
    let lisp_fn_args = lisp_attr::parse(&attr_ts.to_string(), &function).unwrap();

    let mut cargs = quote::Tokens::new();
    let mut rargs = quote::Tokens::new();
    let mut body = quote::Tokens::new();
    let max_args = function.args.len() as i16;
    match function.fntype {
        function::LispFnType::Normal(_) => {
            for ident in function.args {
                let arg = quote! { #ident: ::remacs_sys::Lisp_Object, };
                cargs.append(arg);

                let arg = quote! { ::lisp::LispObject::from_raw(#ident), };
                rargs.append(arg);
            }
        }
        function::LispFnType::Many => {
            let args =
                quote! {
                nargs: ::libc::ptrdiff_t,
                args: *mut ::remacs_sys::Lisp_Object,
            };
            cargs.append(args);

            let b =
                quote! {
                let args = unsafe {
                    ::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(
                        args, nargs as usize)
                };
            };
            body.append(b);

            let arg = quote! { unsafe { ::std::mem::transmute(args) } };
            rargs.append(arg);
        }
    }

    let cname = lisp_fn_args.c_name;
    let sname = concat_idents("S", &cname);
    let fname = concat_idents("F", &cname);
    let rname = function.name;
    let min_args = lisp_fn_args.min;
    let max_args = match function.fntype {
        function::LispFnType::Normal(_) => quote! { #max_args },
        function::LispFnType::Many => quote! { ::lisp::MANY  },
    };
    let symbol_name = CByteLiteral(&lisp_fn_args.name);

    let tokens =
        quote! {
        #[no_mangle]
        pub extern "C" fn #fname(#cargs) -> ::remacs_sys::Lisp_Object {
            #body

            let ret = #rname(#rargs);
            ret.to_raw()
        }

        lazy_static! {
            pub static ref #sname: ::remacs_sys::Lisp_Subr = ::remacs_sys::Lisp_Subr {
                header: ::remacs_sys::Lisp_Vectorlike_Header {
                    size: (::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
                        << ::remacs_sys::PSEUDOVECTOR_AREA_BITS,
                },
                function: self::#fname as *const ::libc::c_void,
                min_args: #min_args,
                max_args: #max_args,
                symbol_name: (#symbol_name).as_ptr() as *const ::libc::c_char,
                intspec: ::std::ptr::null(),
                doc: ::std::ptr::null(),
            };
        }

        #fn_item
    };

    TokenStream::from_str(tokens.as_str()).unwrap()
}

struct CByteLiteral<'a>(&'a str);

impl<'a> quote::ToTokens for CByteLiteral<'a> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        tokens.append(&format!(r#"b"{}\0""#, self.0));
    }
}

fn concat_idents(lhs: &str, rhs: &str) -> syn::Ident {
    syn::Ident::new(format!("{}{}", lhs, rhs))
}
