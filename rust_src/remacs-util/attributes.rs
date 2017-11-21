//! Parse the #[lisp_fn] macro.

use std::fmt::Display;

use darling::FromMetaItem;
use syn;

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
    fn convert<D>(self, def_name: &D, def_min_args: i16) -> Result<LispFnArgs, String>
    where
        D: Display + ?Sized,
    {
        Ok(LispFnArgs {
            name: self.name
                .unwrap_or_else(|| def_name.to_string().replace("_", "-")),
            c_name: self.c_name.unwrap_or_else(|| def_name.to_string()),
            min: if let Some(s) = self.min {
                s.parse()
                    .map_err(|_| "invalid \"min\" number of arguments")?
            } else {
                def_min_args
            },
            intspec: self.intspec,
        })
    }
}

pub struct LispFnArgs {
    pub name: String,
    pub c_name: String,
    pub min: i16,
    pub intspec: Option<String>,
}

pub fn parse_lisp_fn<D>(src: &str, def_name: &D, def_min_args: i16) -> Result<LispFnArgs, String>
where
    D: Display + ?Sized,
{
    if src.is_empty() || src == "#[lisp_fn]" {
        // from_meta_item doesn't accept this simple form...
        LispFnArgsRaw::default().convert(def_name, def_min_args)
    } else {
        // We either get a full "#[lisp_fn(...)]" line or just the parenthesized part
        let src = if src.starts_with("#[") {
            src.to_string()
        } else {
            format!("#[lisp_fn{}]", src)
        };
        syn::parse_outer_attr(&src)
            .and_then(|v| {
                LispFnArgsRaw::from_meta_item(&v.value).map_err(|e| e.to_string())
            })
            .and_then(|v| v.convert(def_name, def_min_args))
    }
}
