use span::Span;

use crate::{Prototypical, Token};

/// Wrapper struct to enable storing span information separately to token
/// structure
#[derive(Debug, Copy, Clone)]
pub struct TokenAndSpan<T: Token> {
    #[allow(missing_docs)]
    pub token: T,
    #[allow(missing_docs)]
    pub span: Span,
}

impl<T: Token> Token for TokenAndSpan<T> {
    type Type = T::Type;
    fn typ(&self) -> Self::Type {
        self.token.typ()
    }
    fn payload(&self) -> String {
        self.token.payload()
    }
    fn span(&self) -> Span {
        self.span
    }
}

impl<T: Token> Prototypical for TokenAndSpan<T> {
    type Prototype = T;
    fn prototype(&self) -> &Self::Prototype {
        &self.token
    }
}
