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
#![allow(clippy::struct_field_names)]

use proc::util::CrateAnd;

mod derive_token;
mod free;
mod infix;
mod keywords;
mod prefix;
mod prototype;
mod utils;

/// Derive macro for pratt::Token
#[proc::derive(name = Token, attribute = pratt, host = "pratt")]
pub fn derive_token(
    crate_: proc::Path,
    input: proc::ItemEnum,
) -> proc::Result<derive_token::DeriveToken> {
    derive_token::DeriveToken::new(crate_, input)
}

/// Pratt prefix parser
#[proc::attribute(host = "pratt")]
pub fn prefix(
    crate_: proc::Path,
    input: proc::ItemFn,
) -> proc::Result<prefix::Prefix> {
    prefix::Prefix::new(&crate_, input)
}

/// Pratt infix parser
#[proc::attribute(host = "pratt")]
pub fn infix(
    crate_: proc::Path,
    input: proc::ItemFn,
) -> proc::Result<infix::Infix> {
    infix::Infix::new(&crate_, input)
}

/// Provide pratt parsing utilities with minimal requirements
#[proc::attribute(host = "pratt")]
pub fn free(
    crate_: proc::Path,
    input: proc::ItemFn,
) -> proc::Result<free::Free> {
    free::Free::new(&crate_, input)
}

// Documented in the wrapper in pratt
#[allow(missing_docs)]
#[proc::function]
pub fn prototype(
    CrateAnd { crate_, args }: CrateAnd<prototype::Input>,
) -> proc::Result<prototype::Prototype> {
    Ok(prototype::Prototype::new(crate_, args))
}

// Documented in the wrapper in pratt
#[allow(missing_docs)]
#[proc::function]
pub fn keywords(args: keywords::Input) -> proc::Result<keywords::Keywords> {
    keywords::Keywords::new(args)
}
