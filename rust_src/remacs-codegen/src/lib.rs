#![crate_type="dylib"]
#![feature(plugin_registrar, rustc_private, i128_type, quote)]

//! remacs-codegen
//! --------------
//! Remacs code generation

extern crate syntax;
extern crate rustc_plugin;

pub mod decorators;
pub mod parser;
pub mod utils;

use syntax::ext::base::SyntaxExtension;
use syntax::symbol::Symbol;
use rustc_plugin::Registry;

/// Register decorators, E. g.: `#[lisp_function]`
macro_rules! register_decorators {
    ($registry:expr; $($name:expr => $func:ident),+) => {
        $($registry.register_syntax_extension(Symbol::intern($name),
                SyntaxExtension::MultiModifier(Box::new(decorators::$func)));
         )+
    }
}

pub const FN_SYMBOL_PREFIX: &'static str = "S";
pub const FN_FFI_PREFIX: &'static str = "F";

#[doc(hidden)]
#[plugin_registrar]
pub fn plugin_registrar(registry: &mut Registry) {
    register_decorators![
        registry;
        "lisp_function" => lisp_function
    ];
}
