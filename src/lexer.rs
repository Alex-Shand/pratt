//! Helpers for building pratt compatibile lexers

use std::marker::PhantomData;

use crate::Lexer;

pub mod builder;

pub use pratt_derive::keywords;

/// Iterate tokens from a lexer, ignoring context
pub fn iter<C: Default, L: Lexer<Context = C>>(
    lexer: &mut L,
) -> impl Iterator<Item = L::Token> {
    struct Iter<'a, C: Default, L: Lexer<Context = C>>(&'a mut L);
    impl<C: Default, L: Lexer<Context = C>> Iterator for Iter<'_, C, L> {
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

#[cfg(test)]
mod tests {
    use builder::StatelessLexerSingleTokenBuilder;
    use span::Chars;

    use super::*;

    #[derive(Debug, Clone, PartialEq, pratt_derive::Token)]
    enum Token {
        #[pratt(payload = "one")]
        One,
        #[pratt(payload = "two")]
        Two,
        #[pratt(payload = "three")]
        Three,
        #[pratt(payload = "or")]
        Or,
        #[pratt(payload = "orange")]
        Orange,
        #[pratt(payload = it.clone())]
        Ident(String),
    }

    fn token_fn(chars: &mut Chars, (): ()) -> Option<Token> {
        macro_rules! keywords {
            ($($tt:tt)*) => {
                super::keywords!(chars = chars, match = {$($tt)*})
            }
        }
        while let Some(c) = chars.skip_whitespace() {
            dbg!(c);
            keywords! {
                "one"    [char::is_whitespace] => return Some(Token::One)
                "two"    [char::is_whitespace] => return Some(Token::Two)
                "three"  [char::is_whitespace] => return Some(Token::Three)
                "or"     [char::is_whitespace] => return Some(Token::Or)
                "orange" [char::is_whitespace] => return Some(Token::Orange)
            }
            if c.is_alphabetic() {
                return Some(Token::Ident(
                    chars.peek_while(char::is_alphanumeric).collect(),
                ));
            }
        }
        None
    }

    fn lexer() -> StatelessLexerSingleTokenBuilder<(), Token> {
        builder::<()>().with_token_fn(token_fn)
    }

    #[test]
    fn test() {
        let text = "one two three or orange orchid";
        let mut lexer = lexer().build(text);
        let tokens = iter(&mut lexer).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            [
                Token::One,
                Token::Two,
                Token::Three,
                Token::Or,
                Token::Orange,
                Token::Ident(String::from("orchid"))
            ]
        );
    }
}
