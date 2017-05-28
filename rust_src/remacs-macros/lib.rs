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
    let lisp_fn_args = lisp_attr::parse(attr_ts.to_string().as_str()).unwrap();
    let fn_item = syn::parse_item(fn_ts.to_string().as_str()).unwrap();
    let function = function::parse(fn_item.clone()).unwrap();
    let lisp_fn_type = function::parse_function_type(&function).unwrap();

    let mut cargs = quote::Tokens::new();
    let mut rargs = quote::Tokens::new();
    let mut body = quote::Tokens::new();
    let max_args = function.decl.inputs.len() as i16;
    match lisp_fn_type {
        function::LispFnType::Normal => {
            for fn_arg in function.decl.inputs {
                let ident = function::get_fn_arg_ident(&fn_arg).unwrap();

                let arg = quote! { #ident: ::remacs_sys::Lisp_Object, };
                cargs.append(arg.to_string());

                let arg = quote! { ::lisp::LispObject::from_raw(#ident), };
                rargs.append(arg.to_string());
            }
        },
        function::LispFnType::Many => {
            let args = quote! {
                nargs: ::libc::ptrdiff_t,
                args: *mut ::remacs_sys::Lisp_Object,
            };
            cargs.append(args.to_string());

            let b = quote! {
                let slice = unsafe {
                    ::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(
                        args, nargs as usize)
                };
                let mut args: ::std::vec::Vec<lisp::LispObject> = slice
                    .iter().map(|arg| ::lisp::LispObject::from_raw(*arg)).collect();
            };
            body.append(b.to_string());

            let arg = quote! { args.as_mut_slice() };
            rargs.append(arg.to_string());
        }
    }

    let sname = concat_idents("S", function.name.clone());
    let fname = concat_idents("F", function.name.clone());
    let rname = function.name;
    let min_args = lisp_fn_args.min;
    let max_args = match lisp_fn_type {
        function::LispFnType::Normal => quote! { #max_args },
        function::LispFnType::Many => quote! { ::lisp::MANY  },
    };
    let symbol_name = ByteLiteral(lisp_fn_args.name.as_str());

    let tokens = quote! {
        #[no_mangle]
        pub extern "C" fn #fname(#cargs) -> ::remacs_sys::Lisp_Object {
            #body

            let ret = #rname(#rargs);
            ret.to_raw()
        }

        lazy_static! {
            pub static ref #sname: ::remacs_sys::Lisp_Subr = ::remacs_sys::Lisp_Subr {
                header: ::remacs_sys::vectorlike_header {
                    size: (::remacs_sys::PVEC_SUBR << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) as ::libc::ptrdiff_t,
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

struct ByteLiteral<'a>(&'a str);

impl<'a> quote::ToTokens for ByteLiteral<'a> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        tokens.append(&format!(r#"b"{}\0""#, self.0));
    }
}

fn concat_idents(lhs: &str, rhs: syn::Ident) -> syn::Ident {
    syn::Ident::new(format!("{}{}", lhs, rhs))
}
