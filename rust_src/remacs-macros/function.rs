use syn;

type Result<T> = ::std::result::Result<T, &'static str>;

pub struct Function {
    /// The function name
    pub name: syn::Ident,

    /// The function header
    pub decl: Box<syn::FnDecl>,

    /// The function contents
    pub block: Box<syn::Block>,
}

pub fn parse(item: syn::Item) -> Result<Function> {
    match item.node {
        syn::ItemKind::Fn(decl, unsafety, constness, abi, _, block) => {
            if is_unsafe(unsafety) {
                return Err("lisp functions cannot be `unsafe`");
            }
            
            if is_const(constness) {
                return Err("lisp functions cannot be `const`");
            }

            if !is_rust_abi(abi) {
                return Err("lisp functions can only use \"Rust\" ABI");
            }

            Ok(Function {
                name: item.ident,
                decl: decl,
                block: block,
            })
        },
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

fn is_rust_abi(abi: Option<syn::Abi>) -> bool {
    match abi {
        Some(abi) => {
            match abi {
                syn::Abi::Named(_) => false,
                syn::Abi::Rust => true,
            }
        },
        None => true,
    }
}

pub fn get_fn_arg_ident(fn_arg: &syn::FnArg) -> Result<syn::Ident> {
    match *fn_arg {
        syn::FnArg::Captured(ref pat, _) => {
            match *pat {
                syn::Pat::Ident(_, ref ident, _) => {
                    Ok(ident.clone())
                },
                _ => Err("invalid function argument"),
            }
        },
        _ => Err("invalid function argument"),
    }
}

pub enum LispFnType {
    Normal,
    Many,
}

pub fn parse_function_type(function: &Function) -> Result<LispFnType> {
    let fndecl = function.decl.clone();

    if fndecl.inputs.len() == 0 {
        return Ok(LispFnType::Normal);
    }

    if fndecl.inputs.len() == 1 {
        match fndecl.inputs[0] {
            syn::FnArg::Captured(_, ref ty) => {
                match parse_arg_type(ty) {
                    ArgType::LispObject => {
                        return Ok(LispFnType::Normal);
                    },
                    ArgType::LispObjectSlice => {
                        return Ok(LispFnType::Many);
                    },
                    ArgType::Other => {
                        return Err("lisp functions should contain only `LispObject`s");
                    },
                }
            },
            syn::FnArg::Ignored(ref ty) => {
                match parse_arg_type(ty) {
                    ArgType::LispObject => {
                        return Ok(LispFnType::Normal);
                    },
                    ArgType::LispObjectSlice => {
                        return Ok(LispFnType::Many);
                    },
                    ArgType::Other => {
                        return Err("lisp functions should contain only `LispObject`s");
                    },
                }
            },
            _ => return Err("lisp functions cannot have `self` arguments"),
        }
    }

    if fndecl.inputs.len() > 1 {
        for fnarg in fndecl.inputs {
            match fnarg {
                syn::FnArg::Captured(_, ref ty) => {
                    match parse_arg_type(ty) {
                        ArgType::LispObject => (),
                        ArgType::LispObjectSlice => {
                            return Err("`LispObject` and `[LispObject]` cannot be mixed");
                        },
                        ArgType::Other => {
                            return Err("lisp functions should contain only `LispObject`s");
                        },
                    }
                },
                syn::FnArg::Ignored(ref ty) => {
                    match parse_arg_type(ty) {
                        ArgType::LispObject => (),
                        ArgType::LispObjectSlice => {
                            return Err("`LispObject` and `[LispObject]` cannot be mixed");
                        },
                        ArgType::Other => {
                            return Err("lisp functions should contain only `LispObject`s");
                        },
                    }
                },
                _ => return Err("lisp functions cannot have `self` arguments"),
            }
        }
    }

    Ok(LispFnType::Normal)
}

enum ArgType {
    LispObject,
    LispObjectSlice,
    Other,
}

fn parse_arg_type(fn_arg: &syn::Ty) -> ArgType {
    match *fn_arg {
        syn::Ty::Path(ref qualification, ref path) => {
            if qualification.is_some() {
                ArgType::Other
            } else {
                if is_lisp_object(path) {
                    ArgType::LispObject
                } else {
                    ArgType::Other
                }
            }
        },
        syn::Ty::Rptr(ref lifetime, ref mut_ty) => {
            if lifetime.is_some() {
                ArgType::Other
            } else {
                match mut_ty.mutability {
                    syn::Mutability::Immutable => ArgType::Other,
                    syn::Mutability::Mutable => {
                        match mut_ty.ty {
                            syn::Ty::Slice(ref ty) => {
                                match **ty {
                                    syn::Ty::Path(ref qualification, ref path) => {
                                        if qualification.is_some() {
                                            ArgType::Other
                                        } else {
                                            if is_lisp_object(path) {
                                                ArgType::LispObjectSlice
                                            } else {
                                                ArgType::Other
                                            }
                                        }
                                    },
                                    _ => ArgType::Other,
                                }
                            }
                            _ => ArgType::Other,
                        }
                    },
                }
            }
        },
        _ => ArgType::Other,
    }
}

macro_rules! make_path {
    (:: $($i:ident)::*) => {
        $crate::syn::Path {
            global: true,
            segments: vec![
                $(
                    $crate::syn::PathSegment {
                        ident: $crate::syn::Ident::new(stringify!($i)),
                        parameters: syn::PathParameters::none(),
                    }
                ),*
            ],
        }
    };
    ($($i:ident)::*) => {
        $crate::syn::Path {
            global: false,
            segments: vec![
                $(
                    $crate::syn::PathSegment {
                        ident: $crate::syn::Ident::new(stringify!($i)),
                        parameters: syn::PathParameters::none(),
                    }
                ),*
            ],
        }
    }
}

fn is_lisp_object(path: &syn::Path) -> bool {
    *path == make_path!(::lisp::LispObject) ||
    *path == make_path!(lisp::LispObject) ||
    *path == make_path!(LispObject)
}
