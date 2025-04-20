//! Builder structs for [SeparatedList](super::SeparatedList)

use std::marker::PhantomData;

use super::SeparatedList;
use crate::{LexerHandle, Result};

/// Builder for [SeparatedList]
#[derive(Debug, Copy, Clone)]
pub struct SeparatedListBuilder;

impl SeparatedListBuilder {
    /// The parse_element function is passed the current [LexerHandle] and
    /// should return one element of the list or error
    pub fn parse_element<
        Token: crate::Token,
        Context: Copy,
        Element,
        F: Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    >(
        self,
        parse_element: F,
    ) -> SeparatedListBuilderWithParseElement<Token, Context, Element, F> {
        SeparatedListBuilderWithParseElement {
            _phantom: PhantomData,
            parse_element,
        }
    }
}

/// See [SeparatedListBuilder]
#[derive(Debug)]
pub struct SeparatedListBuilderWithParseElement<
    Token: crate::Token,
    Context: Copy,
    Element,
    ParseElement: Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
> {
    #[expect(clippy::type_complexity)]
    _phantom: PhantomData<
        fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    >,
    parse_element: ParseElement,
}

impl<Token, Context, Element, ParseElement>
    SeparatedListBuilderWithParseElement<Token, Context, Element, ParseElement>
where
    Token: crate::Token,
    Context: Copy,
    ParseElement:
        Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
{
    /// The has_element function is passed the type of the next token in the
    /// lexer if one exists and must determine if there is another element of
    /// the list or if the list is finished.
    pub fn has_element<F: Fn(Token::Type) -> bool>(
        self,
        has_element: F,
    ) -> SeparatedListBuilderWithParseElementAndHasElement<
        Token,
        Context,
        Element,
        ParseElement,
        F,
    > {
        SeparatedListBuilderWithParseElementAndHasElement {
            prev: self,
            has_element,
        }
    }

    /// Shorthand for a positive has_element check. The provided token type here
    /// should *uniquely* indicate that the following sequence of tokens should
    /// be another list element.
    pub fn test_positive(
        self,
        typ: Token::Type,
    ) -> SeparatedListBuilderWithParseElementAndHasElement<
        Token,
        Context,
        Element,
        ParseElement,
        impl Fn(Token::Type) -> bool,
    > {
        self.has_element(move |t| t == typ)
    }

    /// Shorthand for a negative has_element check. The provided token type
    /// should *uniquely* indicate that the list is finished. For most purposes
    /// this is probably the best thing to use
    pub fn test_negative(
        self,
        typ: Token::Type,
    ) -> SeparatedListBuilderWithParseElementAndHasElement<
        Token,
        Context,
        Element,
        ParseElement,
        impl Fn(Token::Type) -> bool,
    > {
        self.has_element(move |t| t != typ)
    }
}

/// See [SeparatedListBuilder]
#[derive(Debug)]
pub struct SeparatedListBuilderWithParseElementAndHasElement<
    Token: crate::Token,
    Context: Copy,
    Element,
    ParseElement: Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    HasElement: Fn(Token::Type) -> bool,
> {
    prev: SeparatedListBuilderWithParseElement<
        Token,
        Context,
        Element,
        ParseElement,
    >,
    has_element: HasElement,
}

impl<Token, Context, Element, ParseElement, HasElement>
    SeparatedListBuilderWithParseElementAndHasElement<
        Token,
        Context,
        Element,
        ParseElement,
        HasElement,
    >
where
    Token: crate::Token,
    Context: Copy,
    ParseElement:
        Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    HasElement: Fn(Token::Type) -> bool,
{
    /// The separator should be an identityless token (e.g comma) which
    /// separates list elements.
    pub fn separator(
        self,
        separator: Token::Type,
    ) -> CompleteSeparatedListBuilder<
        Token,
        Context,
        Element,
        ParseElement,
        HasElement,
    > {
        CompleteSeparatedListBuilder {
            prev: self,
            separator,
        }
    }
}

/// See [SeparatedListBuilder]
#[derive(Debug)]
pub struct CompleteSeparatedListBuilder<
    Token: crate::Token,
    Context: Copy,
    Element,
    ParseElement: Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    HasElement: Fn(Token::Type) -> bool,
> {
    prev: SeparatedListBuilderWithParseElementAndHasElement<
        Token,
        Context,
        Element,
        ParseElement,
        HasElement,
    >,
    separator: Token::Type,
}

impl<Token, Context, Element, ParseElement, HasElement>
    CompleteSeparatedListBuilder<
        Token,
        Context,
        Element,
        ParseElement,
        HasElement,
    >
where
    Token: crate::Token,
    Context: Copy,
    ParseElement:
        Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    HasElement: Fn(Token::Type) -> bool,
{
    /// Finalize builder
    pub fn build(
        self,
    ) -> SeparatedList<Token, Context, Element, ParseElement, HasElement> {
        let CompleteSeparatedListBuilder {
            prev:
                SeparatedListBuilderWithParseElementAndHasElement {
                    prev:
                        SeparatedListBuilderWithParseElement {
                            _phantom,
                            parse_element,
                        },
                    has_element,
                },
            separator,
        } = self;
        SeparatedList {
            _phantom: PhantomData,
            parse_element,
            has_element,
            separator,
        }
    }
}
