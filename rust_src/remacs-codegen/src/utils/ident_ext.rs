use std::fmt::Display;
use syntax::ast::Ident;

pub trait IdentExt {
    fn prepend<T: Display>(&self, other: T) -> Ident;
    fn append<T: Display>(&self, other: T) -> Ident;
}

impl IdentExt for Ident {
    fn prepend<T: Display>(&self, other: T) -> Ident {
        let new_ident = format!("{}{}", other, self.name);
        Ident::from_str(new_ident.as_str())
    }

    fn append<T: Display>(&self, other: T) -> Ident {
        let new_ident = format!("{}{}", self.name, other);
        Ident::from_str(new_ident.as_str())
    }
}
