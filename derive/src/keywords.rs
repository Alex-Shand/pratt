use std::{collections::HashMap, rc::Rc};

use proc::{
    quote::{format_ident, quote, ToTokens},
    syn::{Error, Ident},
    Result,
};

pub(crate) use self::input::Input;
use self::{
    dispatcher::Dispatcher,
    nursary::{Braced, GreedySequence},
};

mod dispatcher;
mod input;
mod nursary;

pub(crate) struct Keywords {
    chars: Ident,
    checkpoint: Rc<Ident>,
    dispatcher: Dispatcher,
}

impl Keywords {
    pub(crate) fn new(input: Input) -> Result<Self> {
        let Input {
            _chars_kw: _,
            _eq1: _,
            chars,
            _comma: _,
            _match_kw: _,
            _eq2: _,
            matchers: Braced(GreedySequence(m)),
        } = input;
        let mut matchers = HashMap::new();
        for matcher in m {
            let str = matcher.word.value().chars().collect();
            let terminator = matcher.terminator.0;
            let body = matcher.body;
            if matchers.insert(str, (body, terminator)).is_some() {
                return Err(Error::new_spanned(
                    matcher.word,
                    "duplicate keyword",
                ));
            }
        }
        let checkpoint = Rc::new(format_ident!("__pratt_internal_checkpoint"));
        Ok(Keywords {
            chars,
            checkpoint: Rc::clone(&checkpoint),
            dispatcher: Dispatcher::new(checkpoint, matchers),
        })
    }
}

impl ToTokens for Keywords {
    fn to_tokens(&self, tokens: &mut proc::TokenStream) {
        let Keywords {
            chars,
            checkpoint,
            dispatcher,
        } = self;
        tokens.extend(quote! {
            {
                let mut #checkpoint = #chars.checkpoint();
                #dispatcher
            }
        });
    }
}
