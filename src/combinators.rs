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
use crate::Lexer;

mod non_empty_separated_list;
mod separated_list;

/// Peek a token, if it matches the specified type advance the lexer and return
/// true, otherwise return false. Returns false if there are no tokens available.
fn check<Token: crate::Token, Context: Copy>(
    tokens: &mut dyn Lexer<Token = Token, Context = Context>,
    context: Context,
    expected: Token::Type,
) -> bool {
    let Some(token) = tokens.peek(context) else {
        return false;
    };
    if token.typ() == expected {
        let _ = tokens.token(context);
        return true;
    }
    false
}
