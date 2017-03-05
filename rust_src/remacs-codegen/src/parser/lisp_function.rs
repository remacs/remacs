//! Parse a `LispFunction` from `MetaItem`.

use std::collections::HashSet;

use syntax::ext::base::ExtCtxt;
use syntax::ext::base::Annotatable;
use syntax::ext::quote::rt::Span;
use syntax::ast::MetaItem;
use syntax::ast::NestedMetaItem;
use syntax::ast::LitKind;

use ::utils::span;
use ::utils::MetaItemExt;
use ::utils::SpanExt;

use super::keyvalue::KVSpanned;
use super::function::Function;

/// Represents a `#[lisp_function(..)]` attribute.
pub struct LispFunction {
    /// Minimum arguments that can be passed to a lisp function.
    pub min_args: u8,

    /// The lisp function name, E. g.: `symbolp`.
    pub name: String,

    /// The Emacs Lisp function.
    pub annotated_fn: Function,
}

impl LispFunction {
    pub fn from(ecx: &mut ExtCtxt,
                span: Span,
                meta_item: &MetaItem,
                annotatable: &Annotatable)
                -> LispFunction {
        let meta_items = meta_item.meta_item_list()
            .unwrap_or_else(|| {
                ecx.struct_span_err(span, "incorrect use of attribute")
                    .help("attributes in Remacs must have the form: #[lisp_function(..)]")
                    .emit();
                ecx.span_fatal(span, "malformed attribute");
            });

        if meta_items.len() < 2 {
            ecx.span_fatal(span,
                           "#[lisp_function(..)] must have at least two paramters");
        }

        let annotated_fn = Function::from(ecx, annotatable);

        let mut seen_keys = HashSet::new();
        let mut name = String::new();
        let mut min = 0;
        for mt_item in meta_items {
            let kv_opt = kv_from_nested(&mt_item);
            if kv_opt.is_none() {
                ecx.span_err(span, "expected key = value");
                continue;
            }

            let kv = kv_opt.unwrap();
            match kv.key().as_str() {
                "name" => {
                    name = match parse_name(ecx, &kv) {
                        Some(name) => name,
                        None => continue,
                    };
                }
                "min" => {
                    min = match parse_min(ecx, &kv) {
                        Some(min) => min,
                        None => continue,
                    }
                }
                _ => {
                    let msg = format!("`{}` is not a known parameter", kv.key());
                    ecx.span_err(kv.span, &msg);
                    continue;
                }
            }

            if seen_keys.contains(kv.key()) {
                let msg = format!("`{}` was already defined", kv.key());
                ecx.struct_span_warn(kv.span, &msg)
                    .note("the last declared value will be used")
                    .emit();
            } else {
                seen_keys.insert(kv.key().clone());
            }
        }

        // TODO: Parse the attribute
        LispFunction {
            min_args: min,
            name: name,
            annotated_fn: annotated_fn,
        }
    }
}

fn parse_name(ecx: &ExtCtxt, kv: &KVSpanned<LitKind>) -> Option<String> {
    if let LitKind::Str(s, _) = *kv.value() {
        Some((&*s.as_str()).to_owned())
    } else {
        ecx.span_err(kv.span, "`name` value must be a string");
        None
    }
}

fn parse_min(ecx: &ExtCtxt, kv: &KVSpanned<LitKind>) -> Option<u8> {
    if let LitKind::Int(n, _) = *kv.value() {
        let max = u8::max_value();
        if n > max as u128 {
            let err = format!("`min` should be less or equal to {}", max);
            ecx.span_err(kv.span, err.as_str());
            None
        } else {
            Some(n as u8)
        }
    } else {
        ecx.span_err(kv.span, "`min` value must be an integer");
        None
    }
}

pub fn kv_from_nested(item: &NestedMetaItem) -> Option<KVSpanned<LitKind>> {
    item.name_value().map(|(name, value)| {
        let k_span = item.span().shorten_to(name.as_str().len());
        KVSpanned {
            key: span(name.to_string(), k_span),
            value: value.clone(),
            span: item.span(),
        }
    })
}
