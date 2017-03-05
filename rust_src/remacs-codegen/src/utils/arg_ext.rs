use syntax::ast::{Arg, PatKind, Ident, Name};

pub trait ArgExt {
    fn ident(&self) -> Option<&Ident>;

    fn name(&self) -> Option<&Name> {
        self.ident().map(|ident| &ident.name)
    }

    fn named(&self, name: &Name) -> bool {
        self.name().map_or(false, |a| a == name)
    }
}

impl ArgExt for Arg {
    fn ident(&self) -> Option<&Ident> {
        match self.pat.node {
            PatKind::Ident(_, ref ident, _) => Some(&ident.node),
            _ => None,
        }
    }
}
