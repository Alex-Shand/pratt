//! Helpers for handling spans
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
#![allow(clippy::missing_errors_doc)]

use std::iter::Peekable;

pub use pratt_derive::{free, infix, prefix, Token};
pub use span;
use span::Span;
pub use table::{error::Error, Assoc, Result, Table};

pub mod combinators;
mod table;

/// Interface for tokens processed by the parser
///
/// # Example
/// ```
/// # use pratt::Token as _;
/// use span::{Span, LineAndColumn};
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
/// let span = Span {
///     start: LineAndColumn { line: 0, column: 0 },
///     end: LineAndColumn { line: 0, column: 0 },
/// };
/// assert_eq!(Token::Int(5, span).span(), span);
/// assert_eq!(Token::Blah.span(), Span::UNKNOWN);
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

pub trait Lexer {
    type Token: Token;
    type Context: Copy;

    fn peek(&mut self, context: Self::Context) -> Option<&Self::Token>;
    fn token(&mut self, context: Self::Context) -> Option<Self::Token>;
}

impl<T: Token, I: Iterator<Item = T>> Lexer for Peekable<I> {
    type Token = T;

    type Context = ();

    fn peek(&mut self, (): Self::Context) -> Option<&Self::Token> {
        self.peek()
    }

    fn token(&mut self, (): Self::Context) -> Option<Self::Token> {
        self.next()
    }
}

pub trait Prototypical {
    type Prototype;
    fn prototype(&self) -> &Self::Prototype;
}

pub fn prototype<P>(p: &impl Prototypical<Prototype = P>) -> &P {
    p.prototype()
}

pub trait Spanned {
    fn span(&self) -> Span;
}

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
    use ::span::{LineAndColumn, Span};
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::Token as _;

    mod token {
        use span::{LineAndColumn, Span};

        // If this compiles we know that #[derive(Token)] doesn't need the token
        // trait in scope

        #[derive(pratt_derive::Token)]
        pub(super) enum Token {
            #[pratt(payload = it.0.to_string(), span = custom_span(it))]
            Int(usize),
            #[pratt(payload = it.0.clone(), span = *it.1)]
            String(String, Span),
            #[pratt(payload = *it.1)]
            Char((), char),
            #[pratt(payload = "Foo", span = Span { start: LineAndColumn { line: 1, column: 2 }, end: LineAndColumn { line: 3, column: 4 } })]
            Foo,
        }

        fn custom_span((i,): (&usize,)) -> Span {
            let pos = LineAndColumn {
                line: *i,
                column: *i,
            };
            Span {
                start: pos,
                end: pos,
            }
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
    #[case(Token::Int(42), Span { start: LineAndColumn { line: 42, column: 42 }, end: LineAndColumn { line: 42, column: 42 } })]
    #[case(
        Token::String(String::new(), Span { start: LineAndColumn { line: 4, column: 3 }, end: LineAndColumn { line: 2, column: 1 } }),
        Span { start: LineAndColumn { line: 4, column: 3 }, end: LineAndColumn { line: 2, column: 1 } }
    )]
    #[case(Token::Foo, Span { start: LineAndColumn { line: 1, column: 2 }, end: LineAndColumn { line: 3, column: 4 } })]
    fn span(#[case] token: Token, #[case] expected: Span) {
        assert_eq!(token.span(), expected);
    }

    #[test]
    fn span_unknown() {
        assert!(Token::Char((), 'c').span().is_unknown());
    }
}
