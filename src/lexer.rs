//! Helpers for building pratt compatibile lexers

use std::marker::PhantomData;

use crate::{Error, Lexer, Token, error_util::format_error};

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
/// # use span::Chars;
/// # use pratt::lexer::Result;
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
/// fn token_fn(chars: &mut Chars, (): ()) -> Result<Option<Token>> {
///     // Capture the input iterator here to avoid specifying it below
///     macro_rules! keywords {
///         ($($tt:tt)*) => {
///             pratt::lexer::keywords!(chars = chars, match = {$($tt)*})
///         }
///     }
///     while let Some(c) = chars.skip_whitespace() {
///         keywords! {
///             "one"    [char::is_whitespace] => return Ok(Some(Token::One))
///             "two"    [char::is_whitespace] => return Ok(Some(Token::Two))
///             "three"  [char::is_whitespace] => return Ok(Some(Token::Three))
///             "or"     [char::is_whitespace] => return Ok(Some(Token::Or))
///             "orange" [char::is_whitespace] => return Ok(Some(Token::Orange))
///         }
///         if c.is_alphabetic() {
///             return Ok(Some(Token::Ident(
///                 chars.peek_while(char::is_alphanumeric).collect(),
///             )));
///         }
///     }
///     Ok(None)
/// }
///
/// let text = "one two three or orange orchid";
/// let mut lexer = pratt::lexer::builder().with_token_fn(token_fn).build(text);
/// let tokens = pratt::lexer::iter(&mut lexer).collect::<Result<Vec<_>>>().unwrap();
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
use span::Span;

/// Result type returned by a lexer
pub type Result<T> = std::result::Result<T, LexError>;

/// Error type that the lexer is allowed to return to the parser
#[derive(Debug, thiserror::Error)]
pub enum LexError {
    /// The lexer ran out of characters mid token
    #[error("Unexpected EOF{}", format_error(.0))]
    UnexpectedEOF(Option<String>),
    /// The lexer encountered an unexpected character
    #[error("Unexpected character `{}` at {}{}", .0, .1, format_error(.2))]
    UnexpectedCharacter(char, Span, Option<String>),
    /// Any custom error condition
    #[error(transparent)]
    Custom(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
    /// Same as [LexError::Custom] but also carries a span pointing to where the
    /// error occured
    #[error("{source} at {span}")]
    CustomSpanned {
        #[allow(missing_docs)]
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
        #[allow(missing_docs)]
        span: Span,
    },
}

impl<T: Token> From<LexError> for Error<T> {
    fn from(value: LexError) -> Self {
        match value {
            LexError::UnexpectedEOF(msg) => Error::UnexpectedEOF(msg),
            LexError::UnexpectedCharacter(c, span, msg) => {
                Error::LexerUnexpectedCharacter(c, span, msg)
            }
            LexError::Custom(error) => Error::Custom(error),
            LexError::CustomSpanned { source, span } => {
                Error::CustomSpanned { source, span }
            }
        }
    }
}

impl LexError {
    /// Helper function for constructing [LexError::UnexpectedEOF] with a set of
    /// expected characters
    #[must_use]
    pub fn unexpected_eof(expected: Vec<char>) -> Self {
        let msg = format_expected(expected);
        LexError::UnexpectedEOF(Some(msg))
    }

    /// Helper function for constructing [LexError::UnexpectedEOF] with a
    /// message
    pub fn unexpected_eof_msg(msg: impl Into<String>) -> Self {
        LexError::UnexpectedEOF(Some(msg.into()))
    }

    /// Helper function for constructing [LexError::UnexpectedCharacter] with a
    /// set of expected characters
    #[must_use]
    pub fn unexpected_char(c: char, span: Span, expected: Vec<char>) -> Self {
        let msg = format_expected(expected);
        LexError::UnexpectedCharacter(c, span, Some(msg))
    }

    /// Helper function for constructing [LexError::UnexpectedCharacter] with a
    /// message
    pub fn unexpected_char_msg(
        c: char,
        span: Span,
        msg: impl Into<String>,
    ) -> Self {
        LexError::UnexpectedCharacter(c, span, Some(msg.into()))
    }

    /// Helper function for constructing [LexError::Custom]
    pub fn custom(
        e: impl Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
    ) -> Self {
        LexError::Custom(e.into())
    }

    /// Helper function for constructing [LexError::Custom]
    pub fn custom_spanned(
        e: impl Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
        span: Span,
    ) -> Self {
        LexError::CustomSpanned {
            source: e.into(),
            span,
        }
    }
}

fn format_expected(expected: Vec<char>) -> String {
    if expected.len() == 1 {
        format!("Expected `{}`", expected[0])
    } else {
        format!(
            "Expected one of {}",
            expected
                .into_iter()
                .map(|c| format!("`{c}`"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// Iterate tokens from a lexer, ignoring context
pub fn iter<C: Default, L: Lexer<Context = C>>(
    lexer: &mut L,
) -> impl Iterator<Item = Result<L::Token>> {
    struct Iter<'a, C: Default, L: Lexer<Context = C>>(&'a mut L);
    impl<C: Default, L: Lexer<Context = C>> Iterator for Iter<'_, C, L> {
        type Item = Result<L::Token>;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.token(C::default()).transpose()
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

    #[expect(clippy::unnecessary_wraps)]
    fn token_fn(chars: &mut Chars, (): ()) -> Result<Option<Token>> {
        macro_rules! keywords {
            ($($tt:tt)*) => {
                super::keywords!(chars = chars, match = {$($tt)*})
            }
        }
        while let Some(c) = chars.skip_whitespace() {
            dbg!(c);
            keywords! {
                "one"    [char::is_whitespace] => return Ok(Some(Token::One))
                "two"    [char::is_whitespace] => return Ok(Some(Token::Two))
                "three"  [char::is_whitespace] => return Ok(Some(Token::Three))
                "or"     [char::is_whitespace] => return Ok(Some(Token::Or))
                "orange" [char::is_whitespace] => return Ok(Some(Token::Orange))
            }
            if c.is_alphabetic() {
                return Ok(Some(Token::Ident(
                    chars.peek_while(char::is_alphanumeric).collect(),
                )));
            }
        }
        Ok(None)
    }

    fn lexer() -> StatelessLexerSingleTokenBuilder<(), Token> {
        builder::<()>().with_token_fn(token_fn)
    }

    #[test]
    fn test() -> Result<()> {
        let text = "one two three or orange orchid";
        let mut lexer = lexer().build(text);
        let tokens = iter(&mut lexer).collect::<Result<Vec<_>>>()?;
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
        Ok(())
    }
}
