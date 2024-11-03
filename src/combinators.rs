//! Parser combinators

use do_while::do_while;

use crate::{Lexer, Result};

pub fn separated_list<Token: crate::Token, Context: Copy, Element>(
    parse_element: impl Fn(
        &mut dyn Lexer<Token = Token, Context = Context>,
    ) -> Result<Token, Element>,
    lexer: &mut dyn Lexer<Token = Token, Context = Context>,
    context: Context,
    separator: <Token as crate::Token>::Type,
    terminator: <Token as crate::Token>::Type,
) -> Result<Token, Vec<Element>> {
    let mut result = Vec::new();
    do_while! {
        do {
            if lexer.peek(context).map_or(true, |token| token.typ() == terminator) {
                return Ok(result);
            }
            result.push(parse_element(lexer)?);
        } while check(lexer, context, separator);
    }
    Ok(result)
}

pub fn non_empty_separated_list<Token: crate::Token, Context: Copy, Element>(
    parse_element: impl Fn(
        &mut dyn Lexer<Token = Token, Context = Context>,
    ) -> Result<Token, Element>,
    has_element: impl Fn(&mut dyn Lexer<Token = Token, Context = Context>) -> bool,
    lexer: &mut dyn Lexer<Token = Token, Context = Context>,
    context: Context,
    separator: <Token as crate::Token>::Type,
) -> Result<Token, Vec<Element>> {
    let mut result = vec![parse_element(lexer)?];
    while check(lexer, context, separator) && has_element(lexer) {
        result.push(parse_element(lexer)?);
    }
    Ok(result)
}

fn check<Token: crate::Token, Context: Copy>(
    tokens: &mut dyn Lexer<Token = Token, Context = Context>,
    context: Context,
    expected: <Token as crate::Token>::Type,
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
