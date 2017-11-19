#![feature(proc_macro)]
#![recursion_limit = "128"]

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;
extern crate darling;

use darling::FromMetaItem;
use proc_macro::TokenStream;
use std::str::FromStr;

mod function;
use function::Function;

/// Arguments of the lisp_fn attribute.
#[derive(FromMetaItem, Default)]
struct LispFnArgsRaw {
    /// Desired Lisp name of the function.
    /// If not given, derived as the Rust name with "_" -> "-".
    #[darling(default)]
    name: Option<String>,
    /// Desired C name of the related statics (with F and S appended).
    /// If not given, same as the Rust name.
    #[darling(default)]
    c_name: Option<String>,
    /// Minimum number of required arguments.
    /// If not given, all arguments are required for normal functions,
    /// and no arguments are required for MANY functions.
    #[darling(default)]
    min: Option<String>,
    /// The interactive specification. This may be a normal prompt
    /// string, such as `"bBuffer: "` or an elisp form as a string.
    /// If the function is not interactive, this should be None.
    #[darling(default)]
    intspec: Option<String>,
}

impl LispFnArgsRaw {
    fn convert(self, function: &Function) -> Result<LispFnArgs, String> {
        Ok(LispFnArgs {
            name: self.name.unwrap_or_else(|| function.name.to_string().replace("_", "-")),
            c_name: self.c_name.unwrap_or_else(|| function.name.to_string()),
            min: if let Some(s) = self.min {
                s.parse().unwrap()
            } else {
                function.fntype.def_min_args()
            },
            intspec: self.intspec,
        })
    }
}

struct LispFnArgs {
    name: String,
    c_name: String,
    min: i16,
    intspec: Option<String>,
}

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let fn_item = syn::parse_item(&fn_ts.to_string()).unwrap();
    let function = function::parse(&fn_item).unwrap();
    let lisp_fn_args = if attr_ts.is_empty() {
        LispFnArgsRaw::default().convert(&function).unwrap()
    } else {
        let s = format!("#[lisp_fn{}]", attr_ts);
        let parsed = syn::parse_outer_attr(&s)
            .and_then(|v| LispFnArgsRaw::from_meta_item(&v.value).map_err(|e| e.to_string()))
            .and_then(|v| v.convert(&function));
        match parsed {
            Ok(v) => v,
            Err(e) => panic!("Invalid lisp_fn attribute ({}): {}", s, e),
        }
    };

    let mut cargs = quote::Tokens::new();
    let mut rargs = quote::Tokens::new();
    let mut body = quote::Tokens::new();
    let max_args = function.args.len() as i16;
    let intspec = if let Some(intspec) = lisp_fn_args.intspec {
        let cbyte_intspec = CByteLiteral(intspec.as_str());
        quote!{ (#cbyte_intspec).as_ptr() as *const ::libc::c_char }
    } else {
        quote!{ ::std::ptr::null() }
    };

    match function.fntype {
        function::LispFnType::Normal(_) => for ident in function.args {
            let arg = quote! { #ident: ::remacs_sys::Lisp_Object, };
            cargs.append(arg);

            let arg = quote! { ::lisp::LispObject::from(#ident), };
            rargs.append(arg);
        },
        function::LispFnType::Many => {
            let args = quote! {
                nargs: ::libc::ptrdiff_t,
                args: *mut ::remacs_sys::Lisp_Object,
            };
            cargs.append(args);

            let b = quote! {
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
    let mut windows_header = quote!{};
    let max_args = match function.fntype {
        function::LispFnType::Normal(_) => quote! { #max_args },
        function::LispFnType::Many => quote! { ::lisp::MANY  },
    };
    let symbol_name = CByteLiteral(&lisp_fn_args.name);

    if cfg!(windows) {
        windows_header = quote!{
            | (::std::mem::size_of::<::remacs_sys::Lisp_Subr>()
               / ::std::mem::size_of::<::remacs_sys::EmacsInt>()) as ::libc::ptrdiff_t
        };
    }

    let tokens = quote! {
        #[no_mangle]
        pub extern "C" fn #fname(#cargs) -> ::remacs_sys::Lisp_Object {
            #body

            let ret = #rname(#rargs);
            ret.to_raw()
        }

        lazy_static! {
            pub static ref #sname: ::lisp::LispSubrRef = {
                let subr = ::remacs_sys::Lisp_Subr {
                    header: ::remacs_sys::Lisp_Vectorlike_Header {
                        size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)
                               << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) #windows_header,
                    },
                    function: self::#fname as *const ::libc::c_void,
                    min_args: #min_args,
                    max_args: #max_args,
                    symbol_name: (#symbol_name).as_ptr() as *const ::libc::c_char,
                    intspec: #intspec,
                    doc: ::std::ptr::null(),
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
