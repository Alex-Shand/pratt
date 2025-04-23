//! Helpers for building pratt compatibile lexers

use std::marker::PhantomData;

use crate::Lexer;

pub mod builder;

/// Helper for handling keywords which may collide with each other or more
/// generic tokens.
///
/// Generates code which performs the minimum possible number of comparisons to
/// determine if the characters at the head of the input match one of the
/// possible tokens. If one of the tokens is a proper prefix of another the
/// longest will be chosen if possible. Similarly once a full token has been
/// found the generated code will confim that it is at a valid token break
/// before committing. If none of the proposed tokens match the iterator is not
/// advanced.
///
/// # Syntax
///
/// ```rust,ignore
/// keywords!(chars = <chars iterator>, match = { <arms> })
/// ```
///
/// Arm syntax is
/// ```rust,ignore
/// "token" [is_terminator] => body
/// ```
/// * `"token"` is the character sequence which makes up the token as a string
///    literal
/// * `is_terminator` should be a function of type `fn(char) -> bool` which
///    should return `true` if the passed character is a token break for the
///    token associated with this branch. This may be different for each branch
///    if necessary. See below for more discussion on token breaks.
/// * `body` will typically be a `return` expression returning the correct token
///    type. If it doesn't terminate the function it must evaluate to `()`.
///
/// # Token Breaking
/// In order to detect the case when a token is a proper prefix of a more
/// general token type each match arm needs to specify a classifier function to
/// determine if the character after the token is expected to end is a valid
/// token break.
///
/// A typical example of this is standard programming language keywords and
/// identifiers.
///
/// For example, consider a language being which has an `or` keyword and
/// identifier syntax such that `orchid` is a valid identifier. If the input
/// iterator has `[o, r, c, h, i, d, ...]` at the head, we will read off the `o`
/// and `r` at which point we could emit the `Or` token. However the correct
/// behaviour would be to keep reading the next four characters and produce an
/// `Ident("orchid")` token. To get the correct behaviour we use the
/// `is_terminator` function to speficy that the `Or` token must be followed by
/// whitespace (`char::is_whitespace`) or more flexibly, specify that it may not
/// be followed by a valid identifier character (`|c| !c.is_alphanumeric()`).
/// Either of these approaches would cause the macro to abort the search and
/// fall through to the rest of the lexer where there is presumably some code
/// which will classify valid identifiers and emit the appropriate token.
///
/// # Example
/// ```
/// use span::Chars;
///
/// #[derive(Debug, Clone, PartialEq, pratt::Token)]
/// #[pratt(crate = pratt)]
/// enum Token {
///     #[pratt(payload = "one")]
///     One,
///     #[pratt(payload = "two")]
///     Two,
///     #[pratt(payload = "three")]
///     Three,
///     #[pratt(payload = "or")]
///     Or,
///     #[pratt(payload = "orange")]
///     Orange,
///     #[pratt(payload = it.clone())]
///     Ident(String),
/// }
///
/// fn token_fn(chars: &mut Chars, (): ()) -> Option<Token> {
///     // Capture the input iterator here to avoid specifying it below
///     macro_rules! keywords {
///         ($($tt:tt)*) => {
///             pratt::lexer::keywords!(chars = chars, match = {$($tt)*})
///         }
///     }
///     while let Some(c) = chars.skip_whitespace() {
///         keywords! {
///             "one"    [char::is_whitespace] => return Some(Token::One)
///             "two"    [char::is_whitespace] => return Some(Token::Two)
///             "three"  [char::is_whitespace] => return Some(Token::Three)
///             "or"     [char::is_whitespace] => return Some(Token::Or)
///             "orange" [char::is_whitespace] => return Some(Token::Orange)
///         }
///         if c.is_alphabetic() {
///             return Some(Token::Ident(
///                 chars.peek_while(char::is_alphanumeric).collect(),
///             ));
///         }
///     }
///     None
/// }
///
/// let text = "one two three or orange orchid";
/// let mut lexer = pratt::lexer::builder().with_token_fn(token_fn).build(text);
/// let tokens = pratt::lexer::iter(&mut lexer).collect::<Vec<_>>();
/// assert_eq!(
///     tokens,
///     [
///         Token::One,
///         Token::Two,
///         Token::Three,
///         Token::Or,
///         Token::Orange,
///         Token::Ident(String::from("orchid"))
///     ]
/// );
/// ```
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
