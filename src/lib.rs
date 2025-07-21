//! Parser library
#![warn(elided_lifetimes_in_paths)]
#![warn(missing_docs)]
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
#![allow(clippy::similar_names)]
#![allow(clippy::missing_errors_doc)]

use std::iter::Peekable;

pub use pratt_derive::{Token, free, infix, prefix};
pub use span;
use span::Span;
#[doc(hidden)]
pub use table::Assoc;
pub use table::{Result, Table, error::Error};
pub use token_and_span::TokenAndSpan;

pub mod combinators;
mod error_util;
pub mod lexer;
mod table;
mod token_and_span;

/// Trait object for [Lexer]
pub type LexerHandle<'a, Token, Context> =
    dyn Lexer<Token = Token, Context = Context> + 'a;

/// Interface for tokens processed by the parser
///
/// # Example
/// ```rust
/// # use pratt::Token as _;
/// use span::{Span, Chars};
///
/// #[derive(pratt::Token)]
/// #[pratt(crate = pratt)]
/// enum Token {
///     #[pratt(payload = it.0.to_string(), span = *it.1)]
///     Int(i32, Span),
///     #[pratt(payload = "???")]
///     Blah
/// }
///
/// assert_eq!(Token::Int(5, Span::UNKNOWN).typ(), <Token as pratt::Token>::Type::Int);
/// assert_eq!(Token::Blah.typ(), <Token as pratt::Token>::Type::Blah);
///
/// assert_eq!(Token::Int(5, Span::UNKNOWN).payload().as_str(), "5");
/// assert_eq!(Token::Blah.payload().as_str(), "???");
///
/// let chars = &mut Chars::new("12345");
/// let start = chars.start_token();
/// for _ in chars.take(4) {}
/// let span = chars.end_token(start);
/// assert_eq!(Token::Int(5, span).span(), span);
/// assert!(Token::Blah.span().is_unknown());
/// ```
pub trait Token {
    /// The token type, separate from its payload
    type Type: Eq + Copy + std::hash::Hash;

    /// Extract the token type
    fn typ(&self) -> Self::Type;

    /// Extract the payload and convert to string
    ///
    /// This should be approximately equivalent to the original source code
    /// fragment that tokenized to this token
    fn payload(&self) -> String;

    /// Retrieve the token's span, return [Span::UNKNOWN] if the token doesn't
    /// carry span information
    fn span(&self) -> Span;
}

/// Interface for lexers accepted by the parser
pub trait Lexer {
    /// Token type produced by the lexer
    type Token: Token;
    /// Context
    type Context: Copy;

    /// Peek the next token to be produced. After a call to peek the next call
    /// to token should yield an equivalent token
    fn peek(
        &mut self,
        context: Self::Context,
    ) -> lexer::Result<Option<&Self::Token>>;
    /// Yield the next token
    fn token(
        &mut self,
        context: Self::Context,
    ) -> lexer::Result<Option<Self::Token>>;
}

impl<T: Token, I: Iterator<Item = T>> Lexer for Peekable<I> {
    type Token = T;

    type Context = ();

    fn peek(
        &mut self,
        (): Self::Context,
    ) -> lexer::Result<Option<&Self::Token>> {
        Ok(self.peek())
    }

    fn token(
        &mut self,
        (): Self::Context,
    ) -> lexer::Result<Option<Self::Token>> {
        Ok(self.next())
    }
}

/// Used for matching consistency in parsers
#[doc(hidden)]
pub trait Prototypical {
    type Prototype;
    fn prototype(&self) -> &Self::Prototype;
}

/// Helper so parser macros can call Prototypical::prototype without the trait
/// in scope
#[doc(hidden)]
pub fn prototype<P>(p: &impl Prototypical<Prototype = P>) -> &P {
    p.prototype()
}

/// Trait for items with spans.
pub trait Spanned {
    /// Retrieve an item's span, return [Span::UNKNOWN] if the item doesn't have
    /// a valid span
    fn span(&self) -> Span;
}

/// Helper so parser macros can call Spanned::span without the trait in scope
#[doc(hidden)]
pub fn span_of(s: &impl Spanned) -> Span {
    s.span()
}

impl<T: Token> Spanned for T {
    fn span(&self) -> Span {
        Token::span(self)
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

#[cfg(test)]
mod tests {
    use ::span::{Chars, Span};
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::Token as _;

    fn custom_span(i: usize) -> Span {
        let chars = &mut Chars::new("12345");
        let start = chars.start_token();
        for _ in chars.take(i) {}
        chars.end_token(start)
    }

    mod token {
        use span::Span;

        // If this compiles we know that #[derive(Token)] doesn't need the token
        // trait in scope

        #[derive(pratt_derive::Token)]
        pub(super) enum Token {
            #[pratt(payload = it.to_string(), span = super::custom_span(*it))]
            Int(usize),
            #[pratt(payload = it.0.clone(), span = *it.1)]
            String(String, Span),
            #[pratt(payload = *it.1)]
            Char((), char),
            #[pratt(payload = "Foo")]
            Foo,
        }
    }
    use token::Token;
    type TokenType = <Token as super::Token>::Type;

    #[rstest]
    #[case(Token::Int(42), TokenType::Int)]
    #[case(Token::String(String::new(), Span::UNKNOWN), TokenType::String)]
    #[case(Token::Char((), 'c'), TokenType::Char)]
    #[case(Token::Foo, TokenType::Foo)]
    fn typ(#[case] token: Token, #[case] expected: TokenType) {
        assert_eq!(token.typ(), expected);
    }

    #[rstest]
    #[case(Token::Int(42), "42")]
    #[case(
        Token::String(String::from("This is a test"), Span::UNKNOWN),
        "This is a test"
    )]
    #[case(Token::Char((), 'c'), "c")]
    #[case(Token::Foo, "Foo")]
    fn payload(#[case] token: Token, #[case] expected: &'static str) {
        assert_eq!(token.payload(), expected);
    }

    #[rstest]
    #[case(Token::Int(4), "line 1 column 1 to column 5")]
    #[case(
        Token::String(String::new(), custom_span(5)),
        "line 1 column 1 to column 6"
    )]
    fn span(#[case] token: Token, #[case] expected: &str) {
        assert_eq!(format!("{:#}", token.span()), expected);
    }

    #[test]
    fn span_unknown() {
        assert!(Token::Char((), 'c').span().is_unknown());
    }
}
