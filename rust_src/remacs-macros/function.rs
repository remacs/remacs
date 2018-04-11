use syn;

type Result<T> = ::std::result::Result<T, &'static str>;

pub enum LispFnType {
    /// A normal function with given max. number of arguments
    Normal(i16),
    /// A function taking an arbitrary amount of arguments as a slice
    Many,
}

impl LispFnType {
    pub fn def_min_args(&self) -> i16 {
        match *self {
            LispFnType::Normal(n) => n,
            LispFnType::Many => 0,
        }
    }
}

pub struct Function {
    /// The function name
    pub name: syn::Ident,

    /// The argument type
    pub fntype: LispFnType,

    /// The function header
    pub args: Vec<syn::Ident>,
}

pub fn parse(item: &syn::Item) -> Result<Function> {
    match *item {
        syn::Item::Fn(syn::ItemFn {
            ref decl,
            ref unsafety,
            ref constness,
            ref abi,
            ref ident,
            ..
        }) => {
            if unsafety.is_some() {
                return Err("lisp functions cannot be `unsafe`");
            }

            if constness.is_some() {
                return Err("lisp functions cannot be `const`");
            }

            if !is_rust_abi(abi) {
                return Err("lisp functions can only use \"Rust\" ABI");
            }

            let args = decl.inputs
                .iter()
                .map(get_fn_arg_ident_ty)
                .collect::<Result<_>>()?;

            Ok(Function {
                name: ident.clone(),
                fntype: parse_function_type(&decl)?,
                args: args,
            })
        }
        _ => Err("`lisp_fn` attribute can only be used on functions"),
    }
}

fn is_rust_abi(abi: &Option<syn::Abi>) -> bool {
    match *abi {
        Some(syn::Abi { name: Some(_), .. }) => false,
        Some(syn::Abi { name: None, .. }) => true,
        None => true,
    }
}

fn get_fn_arg_ident_ty(fn_arg: &syn::FnArg) -> Result<syn::Ident> {
    match *fn_arg {
        syn::FnArg::Captured(syn::ArgCaptured { ref pat, .. }) => match *pat {
            syn::Pat::Ident(syn::PatIdent { ref ident, .. }) => Ok(ident.clone()),
            _ => Err("invalid function argument"),
        },
        _ => Err("invalid function argument"),
    }
}

fn parse_function_type(fndecl: &syn::FnDecl) -> Result<LispFnType> {
    let nargs = fndecl.inputs.len() as i16;
    for fnarg in &fndecl.inputs {
        match *fnarg {
            syn::FnArg::Captured(syn::ArgCaptured { ref ty, .. }) | syn::FnArg::Ignored(ref ty) => {
                match parse_arg_type(ty) {
                    ArgType::LispObject => {}
                    ArgType::LispObjectSlice => {
                        if fndecl.inputs.len() != 1 {
                            return Err("`[LispObject]` cannot be mixed in with other types");
                        }
                        return Ok(LispFnType::Many);
                    }
                    ArgType::Other => {}
                }
            }
            _ => return Err("lisp functions cannot have `self` arguments"),
        }
    }
    Ok(LispFnType::Normal(nargs))
}

enum ArgType {
    LispObject,
    LispObjectSlice,
    Other,
}

fn parse_arg_type(fn_arg: &syn::Type) -> ArgType {
    match *fn_arg {
        syn::Type::Path(syn::TypePath {
            qself: ref qualification,
            ref path,
        }) => if qualification.is_some() {
            ArgType::Other
        } else {
            if is_lisp_object(&path) {
                ArgType::LispObject
            } else {
                ArgType::Other
            }
        },
        syn::Type::Reference(syn::TypeReference {
            elem: ref ty,
            ref lifetime,
            ref mutability,
            ..
        }) => if lifetime.is_some() {
            ArgType::Other
        } else {
            match *mutability {
                None => ArgType::Other,
                Some(_) => match **ty {
                    syn::Type::Slice(syn::TypeSlice { elem: ref ty, .. }) => match **ty {
                        syn::Type::Path(syn::TypePath {
                            qself: ref qualification,
                            ref path,
                        }) => if qualification.is_some() {
                            ArgType::Other
                        } else {
                            if is_lisp_object(&path) {
                                ArgType::LispObjectSlice
                            } else {
                                ArgType::Other
                            }
                        },
                        _ => ArgType::Other,
                    },
                    _ => ArgType::Other,
                },
            }
        },
        _ => ArgType::Other,
    }
}

fn is_lisp_object(path: &syn::Path) -> bool {
    let str_path = format!("{}", quote!(#path));
    str_path == "LispObject" || str_path == "lisp :: LispObject"
        || str_path == ":: lisp :: LispObject"
}
