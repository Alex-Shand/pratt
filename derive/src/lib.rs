//! Derive macro for pratt::Token
#![warn(elided_lifetimes_in_paths)]
#![warn(missing_docs)]
#![warn(noop_method_call)]
#![warn(unreachable_pub)]
#![warn(unused_crate_dependencies)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![deny(unsafe_code)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unused_results)]
#![deny(missing_debug_implementations)]
#![deny(missing_copy_implementations)]
#![warn(clippy::pedantic)]
#![allow(clippy::doc_markdown)]
#![allow(clippy::let_underscore_untyped)]

use proc_macro_crate::{crate_name, FoundCrate};

mod derive_token;
mod free;
mod infix;
mod prefix;
mod util;

/// Derive macro for pratt::Token
#[proc_macro_derive(Token, attributes(pratt))]
pub fn derive_token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_token::derive_token_impl(syn::parse_macro_input!(
        input as syn::ItemEnum
    ))
    .unwrap_or_else(syn::Error::into_compile_error)
    .into()
}

/// Pratt prefix parser
#[proc_macro_attribute]
pub fn prefix(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = {
        let mut parsed = prefix::Args::default();
        let parser = syn::meta::parser(|meta| parsed.parse(&meta));
        syn::parse_macro_input!(args with parser);
        parsed
    };
    prefix::prefix_impl(args, syn::parse_macro_input!(input as syn::ItemFn))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

/// Pratt infix parser
#[proc_macro_attribute]
pub fn infix(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = {
        let mut parsed = infix::Args::default();
        let parser = syn::meta::parser(|meta| parsed.parse(&meta));
        syn::parse_macro_input!(args with parser);
        parsed
    };
    infix::infix_impl(args, syn::parse_macro_input!(input as syn::ItemFn))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

/// Provide pratt parsing utilities with minimal requirements
#[proc_macro_attribute]
pub fn free(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = {
        let mut parsed = free::Args::default();
        let parser = syn::meta::parser(|meta| parsed.parse(&meta));
        syn::parse_macro_input!(args with parser);
        parsed
    };
    free::free_impl(args, syn::parse_macro_input!(input as syn::ItemFn))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn name() -> syn::Result<proc_macro2::TokenStream> {
    match crate_name("pratt") {
        Ok(FoundCrate::Itself) => Ok(quote::quote!(crate)),
        Ok(FoundCrate::Name(name)) => {
            let ident = quote::format_ident!("{name}");
            Ok(quote::quote!(::#ident))
        }
        Err(e) => Err(syn::Error::new(proc_macro2::Span::call_site(), e)),
    }
}
