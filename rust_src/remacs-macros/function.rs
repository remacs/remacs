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
    match item.node {
        syn::ItemKind::Fn(ref decl, unsafety, constness, ref abi, _, _) => {
            if is_unsafe(unsafety) {
                return Err("lisp functions cannot be `unsafe`");
            }

            if is_const(constness) {
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
                name: item.ident.clone(),
                fntype: parse_function_type(&decl)?,
                args: args,
            })
        }
        _ => Err("`lisp_fn` attribute can only be used on functions"),
    }
}

fn is_unsafe(unsafety: syn::Unsafety) -> bool {
    match unsafety {
        syn::Unsafety::Unsafe => true,
        syn::Unsafety::Normal => false,
    }
}

fn is_const(constness: syn::Constness) -> bool {
    match constness {
        syn::Constness::Const => true,
        syn::Constness::NotConst => false,
    }
}

fn is_rust_abi(abi: &Option<syn::Abi>) -> bool {
    match *abi {
        Some(ref abi) => match *abi {
            syn::Abi::Named(_) => false,
            syn::Abi::Rust => true,
        },
        None => true,
    }
}

fn get_fn_arg_ident_ty(fn_arg: &syn::FnArg) -> Result<syn::Ident> {
    match *fn_arg {
        syn::FnArg::Captured(ref pat, _) => match *pat {
            syn::Pat::Ident(_, ref ident, _) => Ok(ident.clone()),
            _ => Err("invalid function argument"),
        },
        _ => Err("invalid function argument"),
    }
}

fn parse_function_type(fndecl: &syn::FnDecl) -> Result<LispFnType> {
    let nargs = fndecl.inputs.len() as i16;
    for fnarg in &fndecl.inputs {
        match *fnarg {
            syn::FnArg::Captured(_, ref ty) | syn::FnArg::Ignored(ref ty) => {
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

fn parse_arg_type(fn_arg: &syn::Ty) -> ArgType {
    match *fn_arg {
        syn::Ty::Path(ref qualification, ref path) => if qualification.is_some() {
            ArgType::Other
        } else {
            if is_lisp_object(path) {
                ArgType::LispObject
            } else {
                ArgType::Other
            }
        },
        syn::Ty::Rptr(ref lifetime, ref mut_ty) => if lifetime.is_some() {
            ArgType::Other
        } else {
            match mut_ty.mutability {
                syn::Mutability::Immutable => ArgType::Other,
                syn::Mutability::Mutable => match mut_ty.ty {
                    syn::Ty::Slice(ref ty) => match **ty {
                        syn::Ty::Path(ref qualification, ref path) => if qualification.is_some() {
                            ArgType::Other
                        } else {
                            if is_lisp_object(path) {
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
