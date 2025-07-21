use std::marker::PhantomData;

pub use self::builders::NonEmptySeparatedListBuilder;
use crate::{LexerHandle, Result};

pub mod builders;

/// Given a parser for an element, construct a new parser which parses a
/// non-empty list of elements
#[expect(missing_debug_implementations)]
pub struct NonEmptySeparatedList<
    Token: crate::Token,
    Context: Copy,
    Element,
    ParseElement: Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    HasElement: Fn(Token::Type) -> bool,
> {
    #[expect(clippy::type_complexity)]
    _phantom: PhantomData<(
        fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
        fn(Token::Type) -> bool,
    )>,
    parse_element: ParseElement,
    has_element: HasElement,
    separator: Token::Type,
}

impl<Token, Context, Element, ParseElement, HasElement>
    NonEmptySeparatedList<Token, Context, Element, ParseElement, HasElement>
where
    Token: crate::Token,
    Context: Copy,
    ParseElement:
        Fn(&mut LexerHandle<'_, Token, Context>) -> Result<Token, Element>,
    HasElement: Fn(Token::Type) -> bool,
{
    #[allow(missing_docs)]
    pub fn parse(
        &self,
        lexer: &mut LexerHandle<'_, Token, Context>,
        context: Context,
    ) -> Result<Token, Vec<Element>> {
        let mut result = vec![(self.parse_element)(lexer)?];
        while super::check(lexer, context, self.separator)?
            && lexer
                .peek(context)?
                .is_some_and(|token| (self.has_element)(token.typ()))
        {
            result.push((self.parse_element)(lexer)?);
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::{NonEmptySeparatedList, NonEmptySeparatedListBuilder};
    use crate::{Error, LexerHandle, Result};

    #[derive(Debug, Copy, Clone, pratt_derive::Token)]
    enum Token {
        #[pratt(payload = "Item")]
        Item,
        #[pratt(payload = ',')]
        Comma,
        #[pratt(payload = "NotItem")]
        NotItem,
    }
    type TokenType = <Token as crate::Token>::Type;

    struct Item;

    #[expect(clippy::type_complexity)]
    fn parser() -> NonEmptySeparatedList<
        Token,
        (),
        Item,
        impl Fn(&mut LexerHandle<'_, Token, ()>) -> Result<Token, Item>,
        impl Fn(TokenType) -> bool,
    > {
        NonEmptySeparatedListBuilder
            .parse_element(|lexer| match lexer.token(())? {
                Some(Token::Item) => Ok(Item),
                Some(tok) => Err(Error::UnexpectedToken(tok, None)),
                None => Err(Error::UnexpectedEOF(None)),
            })
            .test_negative(TokenType::NotItem)
            .separator(TokenType::Comma)
            .build()
    }

    #[test]
    fn parse_success() -> Result<Token, ()> {
        let parser = parser();
        let mut tokens = {
            use Token::*;
            vec![Item, Comma, Item, Comma, Item, NotItem]
                .into_iter()
                .peekable()
        };
        let result = parser.parse(&mut tokens, ())?;
        assert_eq!(3, result.len());
        assert!(matches!(tokens.next(), Some(Token::NotItem)));
        Ok(())
    }

    #[test]
    fn trailing_separator() -> Result<Token, ()> {
        let parser = parser();
        let mut tokens = {
            use Token::*;
            vec![Item, Comma, Item, Comma, Item, Comma, NotItem]
                .into_iter()
                .peekable()
        };
        let result = parser.parse(&mut tokens, ())?;
        assert_eq!(3, result.len());
        assert!(matches!(tokens.next(), Some(Token::NotItem)));
        Ok(())
    }

    #[test]
    fn empty() {
        let parser = parser();
        let mut tokens = vec![].into_iter().peekable();
        let result = parser.parse(&mut tokens, ());
        assert!(matches!(result, Err(Error::UnexpectedEOF(None))));
    }

    #[test]
    fn terminator_only() {
        let parser = parser();
        let mut tokens = vec![Token::NotItem].into_iter().peekable();
        let result = parser.parse(&mut tokens, ());
        assert!(matches!(
            result,
            Err(Error::UnexpectedToken(Token::NotItem, None))
        ));
    }

    #[test]
    fn stray_separator() {
        let parser = parser();
        let mut tokens =
            vec![Token::Comma, Token::NotItem].into_iter().peekable();
        let result = parser.parse(&mut tokens, ());
        assert!(matches!(
            result,
            Err(Error::UnexpectedToken(Token::Comma, None))
        ));
    }
}
