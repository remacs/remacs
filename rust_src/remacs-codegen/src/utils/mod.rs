use syntax::codemap::DUMMY_SP;
use syntax::codemap::spanned;
use syntax::codemap::Spanned;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::ToTokens;
use syntax::tokenstream::TokenTree;
use syntax::parse::token::Token;

pub fn span<T>(t: T, span: Span) -> Spanned<T> {
    spanned(span.lo, span.hi, t)
}

pub fn sep_by_tok<T>(ecx: &ExtCtxt, things: &[T], token: Token) -> Vec<TokenTree>
    where T: ToTokens
{
    let mut output: Vec<TokenTree> = vec![];
    for (i, thing) in things.iter().enumerate() {
        output.extend(thing.to_tokens(ecx));
        if i < things.len() - 1 {
            output.push(TokenTree::Token(DUMMY_SP, token.clone()));
        }
    }

    output
}

mod meta_item_ext;
pub use self::meta_item_ext::*;

mod span_ext;
pub use self::span_ext::*;

mod arg_ext;
pub use self::arg_ext::*;

mod ident_ext;
pub use self::ident_ext::*;
