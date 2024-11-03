use proc_macro2::TokenStream;
use quote::{quote, ToTokens as _};
use syn::{
    punctuated::Punctuated, token::Comma, Error, FnArg, Ident, ItemFn, Pat,
    Result,
};

use crate::util;

#[derive(Debug, Default)]
pub(crate) struct Args {
    pratt: Option<TokenStream>,
}

impl Args {
    pub(crate) fn parse(
        &mut self,
        meta: &syn::meta::ParseNestedMeta<'_>,
    ) -> Result<()> {
        if meta.path.is_ident("crate") {
            let value = meta.value()?;
            let pratt: Ident = value.parse()?;
            self.pratt = Some(pratt.into_token_stream());
            return Ok(());
        }
        Err(Error::new_spanned(
            meta.input.parse::<TokenStream>()?,
            "Unrecognised pratt argument",
        ))
    }
}

pub(crate) fn free_impl(
    Args { pratt }: Args,
    input: ItemFn,
) -> Result<TokenStream> {
    let pratt = if let Some(pratt) = pratt {
        pratt
    } else {
        super::name()?
    };
    let (lexer, context) = validate_args(&input.sig.inputs)?;
    let attrs = &input.attrs;
    let vis = &input.vis;
    let sig = &input.sig;
    let body = &input.block;
    let utils = util::generate(&pratt, lexer, context);
    Ok(quote! {
        #(#attrs)*
        #vis #sig {
            #utils
            #body
        }
    })
}

fn validate_args(args: &Punctuated<FnArg, Comma>) -> Result<(&Ident, &Ident)> {
    let err = "First argument is expected to be the lexer, second should be the context";
    let mut args_iter = args.iter();
    let (Some(lexer), Some(context)) = (args_iter.next(), args_iter.next())
    else {
        return Err(Error::new_spanned(args, err));
    };
    let lexer = extract_ident(lexer, err)?;
    let context = extract_ident(context, err)?;
    Ok((lexer, context))
}

fn extract_ident<'a>(arg: &'a FnArg, err: &'static str) -> Result<&'a Ident> {
    if let FnArg::Typed(p) = arg {
        if let Pat::Ident(i) = &*p.pat {
            Ok(&i.ident)
        } else {
            Err(Error::new_spanned(arg, err))
        }
    } else {
        Err(Error::new_spanned(arg, err))
    }
}
