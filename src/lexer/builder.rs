//! Lexer builder structs

use std::marker::PhantomData;

use span::Chars;

use crate::{Lexer, Token};

#[expect(missing_docs)]
#[expect(missing_debug_implementations)]
pub struct Builder<C: Copy>(pub(super) PhantomData<C>);

impl<C: Copy> Builder<C> {
    /// Add a state type to the generated lexer
    #[must_use]
    pub fn with_state<S: Default>(self) -> BuilderWithState<C, S> {
        BuilderWithState(PhantomData, PhantomData)
    }

    /// Build a lexer from a function which returns one token at a time
    pub fn with_token_fn<T: Token>(
        self,
        token_fn: fn(&mut Chars, C) -> Option<T>,
    ) -> StatelessLexerSingleTokenBuilder<C, T> {
        StatelessLexerSingleTokenBuilder(token_fn)
    }

    /// Build a lexer from a function which may return multiple tokens at once
    pub fn with_tokens_fn<T: Token>(
        self,
        tokens_fn: fn(&mut Chars, C) -> Vec<T>,
    ) -> StatelessLexerMultiTokenBuilder<C, T> {
        StatelessLexerMultiTokenBuilder(tokens_fn)
    }
}

#[expect(missing_docs)]
#[expect(missing_debug_implementations)]
pub struct BuilderWithState<C: Copy, S: Default>(
    PhantomData<C>,
    PhantomData<S>,
);

impl<C: Copy, S: Default> BuilderWithState<C, S> {
    /// Build a lexer from a function which returns one token at a time
    pub fn with_token_fn<T: Token>(
        self,
        token_fn: fn(&mut Chars, C, &mut S) -> Option<T>,
    ) -> StatefullLexerSingleTokenBuilder<C, S, T> {
        StatefullLexerSingleTokenBuilder(token_fn)
    }

    /// Build a lexer from a function which may return multiple tokens at once
    pub fn with_tokens_fn<T: Token>(
        self,
        tokens_fn: fn(&mut Chars, C, &mut S) -> Vec<T>,
    ) -> StatefullLexerMultiTokenBuilder<C, S, T> {
        StatefullLexerMultiTokenBuilder(tokens_fn)
    }
}

#[expect(missing_docs)]
#[expect(missing_debug_implementations)]
pub struct StatelessLexerSingleTokenBuilder<C: Copy, T: Token>(
    fn(&mut Chars, C) -> Option<T>,
);

impl<C: Copy, T: Token> StatelessLexerSingleTokenBuilder<C, T> {
    /// Build the lexer
    pub fn build(
        self,
        str: impl Into<String>,
    ) -> impl Lexer<Token = T, Context = C> {
        struct Lexer<T, C> {
            generator: fn(&mut Chars, C) -> Option<T>,
            chars: Chars,
            token: Option<T>,
            finished: bool,
        }
        impl<T: Token, C: Copy> self::Lexer for Lexer<T, C> {
            type Token = T;

            type Context = C;

            fn token(&mut self, context: Self::Context) -> Option<Self::Token> {
                if self.finished {
                    return None;
                }
                if let Some(tok) = self.token.take() {
                    return Some(tok);
                }
                let token = (self.generator)(&mut self.chars, context);
                if token.is_none() {
                    self.finished = true;
                    return None;
                }
                token
            }

            fn peek(&mut self, context: Self::Context) -> Option<&Self::Token> {
                if self.finished {
                    return None;
                }
                let token = self.token(context)?;
                self.token = Some(token);
                self.token.as_ref()
            }
        }
        Lexer {
            generator: self.0,
            chars: Chars::new(str),
            token: None,
            finished: false,
        }
    }
}

#[expect(missing_docs)]
#[expect(missing_debug_implementations)]
pub struct StatelessLexerMultiTokenBuilder<C: Copy, T: Token>(
    fn(&mut Chars, C) -> Vec<T>,
);

impl<C: Copy, T: Token> StatelessLexerMultiTokenBuilder<C, T> {
    /// Build the lexer
    pub fn build(
        self,
        str: impl Into<String>,
    ) -> impl Lexer<Token = T, Context = C> {
        struct Lexer<T, C> {
            generator: fn(&mut Chars, C) -> Vec<T>,
            chars: Chars,
            queue: Vec<T>,
            finished: bool,
        }
        impl<T: Token, C: Copy> self::Lexer for Lexer<T, C> {
            type Token = T;

            type Context = C;

            fn token(&mut self, context: Self::Context) -> Option<Self::Token> {
                if self.finished {
                    return None;
                }
                if let Some(tok) = self.queue.pop() {
                    return Some(tok);
                }
                let mut tokens = (self.generator)(&mut self.chars, context);
                if tokens.is_empty() {
                    self.finished = true;
                    return None;
                }
                tokens.reverse();
                self.queue = tokens;
                self.token(context)
            }

            fn peek(&mut self, context: Self::Context) -> Option<&Self::Token> {
                if self.finished {
                    return None;
                }
                let token = self.token(context)?;
                self.queue.push(token);
                self.queue.last()
            }
        }
        Lexer {
            generator: self.0,
            chars: Chars::new(str),
            queue: Vec::new(),
            finished: false,
        }
    }
}

#[expect(missing_docs)]
#[expect(missing_debug_implementations)]
pub struct StatefullLexerSingleTokenBuilder<C: Copy, S: Default, T: Token>(
    fn(&mut Chars, C, &mut S) -> Option<T>,
);

impl<C: Copy, S: Default, T: Token> StatefullLexerSingleTokenBuilder<C, S, T> {
    /// Build the lexer
    pub fn build(
        self,
        str: impl Into<String>,
    ) -> impl Lexer<Token = T, Context = C> {
        struct Lexer<T, S, C> {
            generator: fn(&mut Chars, C, &mut S) -> Option<T>,
            chars: Chars,
            token: Option<T>,
            state: S,
            finished: bool,
        }
        impl<T: Token, S, C: Copy> self::Lexer for Lexer<T, S, C> {
            type Token = T;

            type Context = C;

            fn token(&mut self, context: Self::Context) -> Option<Self::Token> {
                if self.finished {
                    return None;
                }
                if let Some(tok) = self.token.take() {
                    return Some(tok);
                }
                let token =
                    (self.generator)(&mut self.chars, context, &mut self.state);
                if token.is_none() {
                    self.finished = true;
                    return None;
                }
                token
            }

            fn peek(&mut self, context: Self::Context) -> Option<&Self::Token> {
                if self.finished {
                    return None;
                }
                let token = self.token(context)?;
                self.token = Some(token);
                self.token.as_ref()
            }
        }
        Lexer {
            generator: self.0,
            chars: Chars::new(str),
            token: None,
            state: S::default(),
            finished: false,
        }
    }
}

#[expect(missing_docs)]
#[expect(missing_debug_implementations)]
pub struct StatefullLexerMultiTokenBuilder<C: Copy, S: Default, T: Token>(
    fn(&mut Chars, C, &mut S) -> Vec<T>,
);

impl<C: Copy, S: Default, T: Token> StatefullLexerMultiTokenBuilder<C, S, T> {
    /// Build the lexer
    pub fn build(
        self,
        str: impl Into<String>,
    ) -> impl Lexer<Token = T, Context = C> {
        struct Lexer<T, S, C> {
            generator: fn(&mut Chars, C, &mut S) -> Vec<T>,
            chars: Chars,
            queue: Vec<T>,
            state: S,
            finished: bool,
        }
        impl<T: Token, S: Default, C: Copy> self::Lexer for Lexer<T, S, C> {
            type Token = T;

            type Context = C;

            fn token(&mut self, context: Self::Context) -> Option<Self::Token> {
                if self.finished {
                    return None;
                }
                if let Some(tok) = self.queue.pop() {
                    return Some(tok);
                }
                let mut tokens =
                    (self.generator)(&mut self.chars, context, &mut self.state);
                if tokens.is_empty() {
                    self.finished = true;
                    return None;
                }
                tokens.reverse();
                self.queue = tokens;
                self.token(context)
            }

            fn peek(&mut self, context: Self::Context) -> Option<&Self::Token> {
                if self.finished {
                    return None;
                }
                let token = self.token(context)?;
                self.queue.push(token);
                self.queue.last()
            }
        }
        Lexer {
            generator: self.0,
            chars: Chars::new(str),
            queue: Vec::new(),
            state: S::default(),
            finished: false,
        }
    }
}
