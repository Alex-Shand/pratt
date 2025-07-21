//! Parser combinators

pub use self::{
    non_empty_separated_list::{
        NonEmptySeparatedList, NonEmptySeparatedListBuilder,
        builders as non_empty_separated_list_builders,
    },
    separated_list::{
        SeparatedList, SeparatedListBuilder,
        builders as separated_list_builders,
    },
};
use crate::{Lexer, lexer};

mod non_empty_separated_list;
mod separated_list;

/// Peek a token, if it matches the specified type advance the lexer and return
/// true, otherwise return false. Returns false if there are no tokens available.
/// Forwards lexer errors out to the caller
fn check<Token: crate::Token, Context: Copy>(
    tokens: &mut dyn Lexer<Token = Token, Context = Context>,
    context: Context,
    expected: Token::Type,
) -> lexer::Result<bool> {
    let Some(token) = tokens.peek(context)? else {
        return Ok(false);
    };
    if token.typ() == expected {
        let _ = tokens.token(context);
        return Ok(true);
    }
    Ok(false)
}
