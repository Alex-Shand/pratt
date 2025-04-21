use proc::{
    quote::ToTokens,
    syn::{Expr, Token, Type},
    Parse, Path,
};
use quote::quote;

#[derive(Parse)]
pub(crate) struct Input {
    target: Type,
    _c1: Token![,],
    prototype: Type,
    _c2: Token![,],
    expr: Expr,
    _c3: Option<Token![,]>,
}

pub(crate) struct Prototype {
    crate_: Path,
    target: Type,
    prototype: Type,
    expr: Expr,
}

impl Prototype {
    pub(crate) fn new(
        crate_: Path,
        Input {
            target,
            _c1,
            prototype,
            _c2,
            expr,
            _c3,
        }: Input,
    ) -> Self {
        Self {
            crate_,
            target,
            prototype,
            expr,
        }
    }
}

impl ToTokens for Prototype {
    fn to_tokens(&self, tokens: &mut proc::TokenStream) {
        let Prototype {
            crate_,
            target,
            prototype,
            expr,
        } = self;
        tokens.extend(quote! {
            impl #crate_::Prototypical for #target {
                type Prototype = #prototype;
                fn prototype(&self) -> &Self::Prototype {
                    #expr
                }
            }
        });
    }
}
