use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

pub(crate) fn generate(
    pratt: &TokenStream,
    lexer: &Ident,
    context: Option<&Ident>,
) -> TokenStream {
    let context = context.map_or_else(|| quote!(()), |i| quote!(#i));
    quote! {
        let mut __pratt_internal_latest_span = #pratt::span::Span::UNKNOWN;
        let mut __pratt_internal_spans: ::std::vec::Vec<#pratt::span::Span> = ::std::vec::Vec::new();

        #[allow(unused_macros)]
        macro_rules! require {
            ($pat:pat => $body:expr, $context:expr, $err:expr) => {
                if let ::std::option::Option::Some(token) = #lexer.token($context) {
                    if let $pat = #pratt::prototype(&token) {
                        let _ = add_span!(#pratt::span_of(&token));
                        $body
                    } else {
                        return Err(#pratt::Error::UnexpectedToken(token, Some(String::from($err))));
                    }
                } else {
                    return Err(#pratt::Error::UnexpectedEOF(Some(String::from($err))));
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
                if let ::std::option::Option::Some(token) = #lexer.peek($context) {
                    if let $pat = #pratt::prototype(token) {
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
            () => { #pratt::span::Span::aggregate(__pratt_internal_spans.as_slice()) }
        }

        #[allow(unused_macros)]
        macro_rules! add_span {
            ($spanned:expr) => { {
                use #pratt::Spanned as _;
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
