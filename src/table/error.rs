use span::Span;

use crate::{Token, error_util::format_error, lexer::LexError};

/// Errors returned from [Table::parse](super::Table::parse) and
/// [Table::parse_at](super::Table::parse_at)
///
/// # Example
/// ```rust
/// # use pratt::Error;
/// use span::{ Span, Chars };
///
/// fn span() -> Span {
///     let chars = &mut Chars::new("123456");
///     let start = chars.start_token();
///     for _ in chars.take(4) {}
///     chars.end_token(start)
/// }
///
/// #[derive(pratt::Token)]
/// #[pratt(crate = pratt)]
/// enum Token {
///     #[pratt(payload = "Test", span = span())]
///     Test
/// }
///
/// let errors = [
///     (Error::UnexpectedEOF(None), "Unexpected EOF"),
///     (Error::UnexpectedEOF(Some(String::from("Some message"))), "Unexpected EOF. Some message"),
///     (Error::UnexpectedToken(Token::Test, None), "Unexpected token `Test` at line 1 column 1"),
///     (Error::UnexpectedToken(Token::Test, Some(String::from("Some message"))), "Unexpected token `Test` at line 1 column 1. Some message"),
///     (Error::Custom("Custom Error".into()), "Custom Error")
/// ];
///
/// for (err, msg) in errors {
///     assert_eq!(err.to_string(), msg);
/// }
/// ```
#[derive(Debug, thiserror::Error)]
pub enum Error<T: Token> {
    /// The parser has run out of tokens before producing a complete AST
    /// fragment
    #[error("Unexpected EOF{}", format_error(.0))]
    UnexpectedEOF(Option<String>),
    /// The parser has encountered a token in an invalid position
    #[error("Unexpected token `{}` at {}{}", .0.payload(), .0.span(), format_error(.1))]
    UnexpectedToken(T, Option<String>),
    /// Forwarded error from the lexer when it encounters an unexpected
    /// character, see [LexError::UnexpectedCharacter]
    #[error("{}", LexError::UnexpectedCharacter(*.0, *.1, .2.clone()))]
    LexerUnexpectedCharacter(char, Span, Option<String>),
    /// Never directly returned from either of the parser entry points, may be
    /// returned by parser functions to signal custom
    /// errors. [Error](std::error::Error) trait impl redirects to the inner
    /// error in this case
    #[error(transparent)]
    Custom(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
    /// Same as [Error::Custom] but also carries a span pointing to where the
    /// error occured
    #[error("{source} at {span}")]
    CustomSpanned {
        #[allow(missing_docs)]
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
        #[allow(missing_docs)]
        span: Span,
    },
}

impl<T: Token> Error<T> {
    /// Helper function for constructing [Error::Custom]
    pub fn custom(
        e: impl Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
    ) -> Self {
        Error::Custom(e.into())
    }

    /// Helper function for constructing [Error::Custom]
    pub fn custom_spanned(
        e: impl Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
        span: Span,
    ) -> Self {
        Error::CustomSpanned {
            source: e.into(),
            span,
        }
    }
}
