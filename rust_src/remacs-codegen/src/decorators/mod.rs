use syntax::ext::base::ExtCtxt;
use syntax::ext::base::Annotatable;
use syntax::ext::quote::rt::Span;
use syntax::ast::MetaItem;
use syntax::ast::Arg;
use syntax::tokenstream::TokenTree;
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::ast::Expr;

use parser::LispFunction;
use utils::IdentExt;
use utils::ArgExt;
use utils::sep_by_tok;

use ::FN_FFI_PREFIX;

pub fn lisp_function(ecx: &mut ExtCtxt,
                     span: Span,
                     meta_item: &MetaItem,
                     annotatable: Annotatable)
                     -> Vec<Annotatable> {
    let lisp_fn = LispFunction::from(ecx, span, meta_item, &annotatable);
    let mut items = Vec::new();

    emit_ffi_fn(&mut items, ecx, &lisp_fn);

    items.push(annotatable);
    items
}

fn emit_ffi_fn(items: &mut Vec<Annotatable>, ecx: &mut ExtCtxt, lisp_fn: &LispFunction) {
    let user_fn_name = lisp_fn.annotated_fn.ident();
    let ref user_fn_inputs = lisp_fn.annotated_fn.decl().inputs;
    let ffi_fn_name = user_fn_name.prepend(FN_FFI_PREFIX);
    let ffi_fn_args = generate_fn_ffi_args(ecx, &user_fn_inputs);
    let fn_args_wrapped = generate_fn_wrapped_args(ecx, &user_fn_inputs);

    let item = quote_item!(ecx,
        fn $ffi_fn_name($ffi_fn_args) -> ::remacs_sys::Lisp_Object {
            $user_fn_name($fn_args_wrapped).to_raw()
        }
    );

    items.push(Annotatable::Item(item.unwrap()));
}

fn generate_fn_ffi_args(ecx: &ExtCtxt, fn_args: &Vec<Arg>) -> Vec<TokenTree> {
    let ffi_args: Vec<Arg> = fn_args.iter()
        .map(|arg| {
            let mut arg = arg.clone();
            arg.ty = quote_ty!(ecx, ::remacs_sys::Lisp_Object);
            arg
        })
        .collect();

    sep_by_tok(ecx, &ffi_args, Token::Comma)
}

fn generate_fn_wrapped_args(ecx: &ExtCtxt, fn_args: &Vec<Arg>) -> Vec<TokenTree> {
    let wrapped_args: Vec<P<Expr>> = fn_args.iter()
        .map(|arg| {
            let arg_name = arg.ident().cloned().unwrap();
            P(quote_expr!(ecx, ::remacs::lisp::LispObject::from_raw($arg_name) ).unwrap())
        })
        .collect();

    sep_by_tok(ecx, &wrapped_args, Token::Comma)
}
