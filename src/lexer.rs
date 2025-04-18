//! Helpers for building pratt compatibile lexers

use std::marker::PhantomData;

use crate::Lexer;

pub mod builder;

/// Iterate tokens from a lexer, ignoring context
pub fn iter<C: Default, L: Lexer<Context = C>>(
    lexer: L,
) -> impl Iterator<Item = L::Token> {
    struct Iter<C: Default, L: Lexer<Context = C>>(L);
    impl<C: Default, L: Lexer<Context = C>> Iterator for Iter<C, L> {
        type Item = L::Token;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.token(C::default())
        }
    }
    Iter(lexer)
}

/// Build a lexer
#[must_use]
pub fn builder<C: Copy>() -> builder::Builder<C> {
    builder::Builder(PhantomData)
}
