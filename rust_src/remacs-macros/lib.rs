#![feature(proc_macro)]

extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn lisp_fn(_attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    fn_ts
}
