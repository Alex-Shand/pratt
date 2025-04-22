use proc::{
    syn::{Expr, Ident, LitStr, Token},
    Parse,
};

use super::nursary::{Braced, Bracketed, GreedySequence};

mod kw {
    use proc::syn::custom_keyword;

    custom_keyword!(chars);
}

#[derive(Parse)]
pub(crate) struct Input {
    pub(crate) _chars_kw: kw::chars,
    pub(crate) _eq1: Token![=],
    pub(crate) chars: Ident,
    pub(crate) _comma: Token![,],
    pub(crate) _match_kw: Token![match],
    pub(crate) _eq2: Token![=],
    pub(crate) matchers: Braced<GreedySequence<Matcher>>,
}

#[derive(Parse)]
pub(crate) struct Matcher {
    pub(crate) word: LitStr,
    pub(crate) terminator: Bracketed<Expr>,
    pub(crate) _arrow: Token![=>],
    pub(crate) body: Expr,
    pub(crate) _comma: Option<Token![,]>,
}
