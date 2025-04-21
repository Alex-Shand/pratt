use proc::{
    quote::{quote, ToTokens},
    syn::{
        punctuated::Punctuated, token::Comma, Attribute, Block, Error, FnArg,
        Ident, Signature, Visibility,
    },
    ItemFn, Path, Result, TokenStream,
};

use crate::utils;

pub(crate) struct Free {
    attrs: Vec<Attribute>,
    vis: Visibility,
    sig: Signature,
    utils: TokenStream,
    body: Block,
}

impl Free {
    pub(crate) fn new(
        crate_: Path,
        ItemFn {
            attrs,
            vis,
            sig,
            block,
        }: ItemFn,
    ) -> Result<Self> {
        let (lexer, context) = Self::validate_args(&sig.inputs)?;
        let utils = utils::generate(&crate_, lexer, context);
        Ok(Self {
            attrs,
            vis,
            sig,
            utils,
            body: *block,
        })
    }

    fn validate_args(
        args: &Punctuated<FnArg, Comma>,
    ) -> Result<(&Ident, Option<&Ident>)> {
        let mut args_iter = args.iter();
        let (Some(lexer), Some(context)) = (args_iter.next(), args_iter.next())
        else {
            return Err(Error::new_spanned(args, "First argument is expected to be the lexer, second should be the context"));
        };
        let lexer = utils::require_lexer_ident(lexer)?;
        let context = utils::extract_ident(context);
        Ok((lexer, context))
    }
}

impl ToTokens for Free {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Free {
            attrs,
            vis,
            sig,
            utils,
            body,
        } = self;
        tokens.extend(quote! {
            #(#attrs)*
            #vis #sig {
                #utils
                #body
            }
        });
    }
}
