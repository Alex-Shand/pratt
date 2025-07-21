use proc::{
    quote::quote,
    syn::{Error, FnArg, Ident, Pat},
    Path, Result, TokenStream,
};

pub(crate) fn generate(
    crate_: &Path,
    lexer: &Ident,
    context: Option<&Ident>,
) -> TokenStream {
    let context = context.map_or_else(|| quote!(()), |i| quote!(#i));
    quote! {
        let mut __pratt_internal_latest_span = #crate_::span::Span::UNKNOWN;
        let mut __pratt_internal_spans: ::std::vec::Vec<#crate_::span::Span> = ::std::vec::Vec::new();

        #[allow(unused_macros)]
        macro_rules! require {
            ($pat:pat => $body:expr, $context:expr, $err:expr) => {
                if let ::std::option::Option::Some(token) = #lexer.token($context)? {
                    if let $pat = #crate_::prototype(&token) {
                        let _ = add_span!(#crate_::span_of(&token));
                        $body
                    } else {
                        return Err(#crate_::Error::UnexpectedToken(token, Some(String::from($err))));
                    }
                } else {
                    return Err(#crate_::Error::UnexpectedEOF(Some(String::from($err))));
                }
            };
            ($pat:pat, $context:expr, $err:expr) => {
                require!($pat => (), $context, $err)
            };
            ($pat:pat => $body:expr, $err:expr) => {
                require!($pat => $body, #context, $err)
            };
            ($pat:pat, $err:expr) => {
                require!($pat, #context, $err)
            };
        }

        #[allow(unused_macros)]
        macro_rules! demand {
            ($pat:pat => $body:expr, $context:expr) => {
                require!($pat => $body, $context, { unreachable!(); #[allow(unreachable_code)] ""})
            };
            ($pat:pat, $context:expr) => {
                demand!($pat => (), $context)
            };
            ($pat:pat => $body:expr) => {
                demand!($pat => $body, #context)
            };
            ($pat:pat) => {
                demand!($pat, #context)
            };
        }

        #[allow(unused_macros)]
        macro_rules! check {
            ($pat:pat, $context:expr) => {
                if let ::std::option::Option::Some(token) = #lexer.peek($context)? {
                    if let $pat = #crate_::prototype(token) {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            };
            ($pat:pat) => {
                check!($pat, #context)
            };
        }

        #[allow(unused_macros)]
        macro_rules! current_span {
            () => { __pratt_internal_latest_span }
        }

        #[allow(unused_macros)]
        macro_rules! span {
            () => { #crate_::span::Span::aggregate(__pratt_internal_spans.as_slice()) }
        }

        #[allow(unused_macros)]
        macro_rules! add_span {
            ($spanned:expr) => { {
                use #crate_::Spanned as _;
                let __pratt_internal_temp = $spanned;
                __pratt_internal_latest_span = __pratt_internal_temp.span();
                __pratt_internal_spans.push(__pratt_internal_latest_span);
                __pratt_internal_temp
            } }
        }

        #[allow(unused_macros)]
        macro_rules! add_spans {
            ($spans:expr) => {
                let _ = $spans.map(|s| add_span!(s)).count();
            }
        }
    }
}

pub(crate) fn extract_ident(arg: &FnArg) -> Option<&Ident> {
    if let FnArg::Typed(p) = arg {
        if let Pat::Ident(i) = &*p.pat {
            return Some(&i.ident);
        }
    }
    None
}

pub(crate) fn require_lexer_ident(arg: &FnArg) -> Result<&Ident> {
    extract_ident(arg).ok_or_else(|| {
        Error::new_spanned(arg, "The lexer argument must be named")
    })
}
