use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::Annotatable;
use syntax::ext::base::ExtCtxt;
use utils::{ArgExt, span};

#[derive(Debug)]
pub struct Function(Spanned<(Ident, FnDecl)>);

impl Function {
    pub fn from(ecx: &mut ExtCtxt, annotated: &Annotatable) -> Function {
        let inner = match *annotated {
            Annotatable::Item(ref item) => {
                match item.node {
                    ItemKind::Fn(ref decl, ..) => {
                        span((item.ident, decl.clone().unwrap()), item.span)
                    }
                    _ => expected_function(ecx, item.span),
                }
            }
            Annotatable::TraitItem(ref item) => expected_function(ecx, item.span),
            Annotatable::ImplItem(ref item) => expected_function(ecx, item.span),
        };

        Function(inner)
    }

    pub fn ident(&self) -> &Ident {
        &self.0.node.0
    }

    pub fn decl(&self) -> &FnDecl {
        &self.0.node.1
    }

    pub fn span(&self) -> Span {
        self.0.span
    }

    pub fn find_input<'a>(&'a self, name: &Name) -> Option<&'a Arg> {
        self.decl().inputs.iter().find(|arg| arg.named(name))
    }
}

fn expected_function(ecx: &mut ExtCtxt, span: Span) -> ! {
    ecx.span_fatal(span, "expected function")
}
