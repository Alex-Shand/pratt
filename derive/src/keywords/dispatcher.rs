use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use proc::{
    quote::{quote, ToTokens},
    syn::{Expr, Ident},
};

#[derive(Debug)]
pub(super) enum Dispatcher {
    AlwaysFail(Rc<Ident>),
    TestThenSucceed {
        checkpoint: Rc<Ident>,
        test: Vec<char>,
        terminator: Expr,
        body: Expr,
    },
    SucceedOrContinue {
        checkpoint: Rc<Ident>,
        next: BTreeMap<char, Dispatcher>,
        terminator: Expr,
        body: Expr,
    },
    Continue(Rc<Ident>, BTreeMap<char, Dispatcher>),
}

impl Dispatcher {
    pub(super) fn new(
        checkpoint: Rc<Ident>,
        matchers: HashMap<Vec<char>, (Expr, Expr)>,
    ) -> Self {
        // If there are no matchers we always fail
        if matchers.is_empty() {
            return Self::AlwaysFail(checkpoint);
        }

        // If there is only one entry we can check the entire remaining string
        // at once and succeed or fail
        if matchers.len() == 1 {
            let (test, (body, terminator)) =
                matchers.into_iter().next().unwrap();
            return Self::TestThenSucceed {
                checkpoint,
                test,
                terminator,
                body,
            };
        }

        // If one of the keys is empty then we *could* immediately succeed if
        // none of the longer matchers are applicable
        if matchers.contains_key(&Vec::new()) {
            let mut matchers = matchers;
            let (body, terminator) = matchers.remove(&Vec::new()).unwrap();
            // Now that we've removed the empty entry, every entry in matchers
            // must have at least one character in its key
            let next = Self::group_by_first_char(&checkpoint, matchers);
            return Self::SucceedOrContinue {
                checkpoint,
                next,
                terminator,
                body,
            };
        }

        // Every entry in matchers has at least one character in it so we have
        // to look at another character
        let next = Self::group_by_first_char(&checkpoint, matchers);
        Self::Continue(checkpoint, next)
    }

    fn group_by_first_char(
        checkpoint: &Rc<Ident>,
        matchers: HashMap<Vec<char>, (Expr, Expr)>,
    ) -> BTreeMap<char, Dispatcher> {
        let mut result: HashMap<char, HashMap<Vec<char>, (Expr, Expr)>> =
            HashMap::new();
        for (key, body) in matchers {
            let (&first, rest) =
                key.split_first().expect("Key should not be empty");
            let rest = rest.to_owned();
            let _ = result.entry(first).or_default().insert(rest, body);
        }
        result
            .into_iter()
            .map(|(c, matchers)| {
                (c, Dispatcher::new(Rc::clone(checkpoint), matchers))
            })
            .collect()
    }
}

impl ToTokens for Dispatcher {
    fn to_tokens(&self, tokens: &mut proc::TokenStream) {
        tokens.extend(match self {
            Dispatcher::AlwaysFail(checkpoint) => quote!(#checkpoint.abort()),
            Dispatcher::TestThenSucceed {
                checkpoint,
                test,
                terminator,
                body,
            } => {
                let bail = Dispatcher::AlwaysFail(Rc::clone(checkpoint));
                let test = test.iter().collect::<String>();
                quote! {
                    // Requirement here is that the head of the iterator matches
                    // the test string and _if_ there is at least one character
                    // after that it matches the terminator predicate. If the
                    // iterator ends exactly on the test string then that counts
                    // & we can run the body
                    if #checkpoint.head_matches(#test) {
                        if let Some(__pratt_internal_c) = #checkpoint.peek() {
                            if (#terminator)(__pratt_internal_c) {
                                #checkpoint.commit();
                                #body
                            } else {
                                #bail
                            }
                        } else {
                            #checkpoint.commit();
                            #body
                        }
                    } else {
                        #bail
                    }
                }
            }
            Dispatcher::SucceedOrContinue {
                checkpoint,
                next,
                terminator,
                body,
            } => {
                let bail = Dispatcher::AlwaysFail(Rc::clone(checkpoint));
                let arms = next.iter().map(
                    |(c, d)| quote! {Some(#c) => { let _ = #checkpoint.next(); #d }},
                );
                quote! {
                    match #checkpoint.peek() {
                        #(#arms),*,
                        // If we reach these arms we haven't seen any characters
                        // which allow us to go for a longer token so we need to
                        // figure out if we can emit the token we currently
                        // have. If there are no more characters (the second
                        // arm) then we can. If there is another character we
                        // have to check if it is a token break.
                        Some(__pratt_internal_c) => if (#terminator)(__pratt_internal_c) {
                            #checkpoint.commit();
                            #body
                        } else {
                            #bail
                        }
                        None => { #body }
                    }
                }
            }
            Dispatcher::Continue(checkpoint, matchers) => {
                let bail = Dispatcher::AlwaysFail(Rc::clone(checkpoint));
                let arms = matchers.iter().map(
                    |(c, d)| quote!(Some(#c) => { let _ = #checkpoint.next(); #d }),
                );
                quote! {
                    match #checkpoint.peek() {
                        #(#arms),*,
                        _ => #bail
                    }
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use proc::{
        quote::format_ident,
        syn::{parse_quote, Expr, Ident},
    };

    use super::Dispatcher;

    fn some_ident() -> Rc<Ident> {
        Rc::new(format_ident!("something"))
    }

    fn some_expr() -> Expr {
        parse_quote!(())
    }

    mod constructor {
        use std::collections::HashMap;

        use super::*;

        #[test]
        fn always_fail() {
            let matchers = HashMap::new();
            assert!(matches!(
                Dispatcher::new(some_ident(), matchers),
                Dispatcher::AlwaysFail(_)
            ));
        }

        #[test]
        fn test_then_succeed() {
            let mut matchers = HashMap::new();
            let _ = matchers
                .insert("test".chars().collect(), (some_expr(), some_expr()));
            assert!(matches!(
                Dispatcher::new(some_ident(), matchers),
                Dispatcher::TestThenSucceed { .. }
            ));
        }

        #[test]
        fn succeed_or_continue() {
            let mut matchers = HashMap::new();
            let _ = matchers.insert(Vec::new(), (some_expr(), some_expr()));
            let _ = matchers
                .insert("a".chars().collect(), (some_expr(), some_expr()));
            assert!(matches!(
                Dispatcher::new(some_ident(), matchers),
                Dispatcher::SucceedOrContinue { .. }
            ));
        }

        #[test]
        fn continue_() {
            let mut matchers = HashMap::new();
            let _ = matchers
                .insert("abc".chars().collect(), (some_expr(), some_expr()));
            let _ = matchers
                .insert("def".chars().collect(), (some_expr(), some_expr()));
            assert!(matches!(
                Dispatcher::new(some_ident(), matchers),
                Dispatcher::Continue(_, _)
            ));
        }
    }

    mod expansion {
        use std::collections::BTreeMap;

        use pretty_assertions::assert_eq;
        use proc::quote::ToTokens;

        use super::*;

        #[test]
        fn always_fail() {
            let dispatcher = Dispatcher::AlwaysFail(some_ident());
            assert_eq!(
                "something . abort ()",
                dispatcher.into_token_stream().to_string()
            );
        }

        #[test]
        fn test_then_succeed() {
            let dispatcher = Dispatcher::TestThenSucceed {
                checkpoint: some_ident(),
                test: "test".chars().collect(),
                terminator: parse_quote!(|c| c == ' '),
                body: some_expr(),
            };
            assert_eq!(
                r#"if something . head_matches ("test") { if let Some (__pratt_internal_c) = something . peek () { if (| c | c == ' ') (__pratt_internal_c) { something . commit () ; () } else { something . abort () } } else { something . commit () ; () } } else { something . abort () }"#,
                dispatcher.into_token_stream().to_string()
            );
        }

        #[test]
        fn succeed_or_continue() {
            let mut next = BTreeMap::new();
            let _ = next.insert('a', Dispatcher::AlwaysFail(some_ident()));
            let _ = next.insert(
                'b',
                Dispatcher::TestThenSucceed {
                    checkpoint: some_ident(),
                    test: "test".chars().collect(),
                    terminator: parse_quote!(char::is_whitespace),
                    body: parse_quote!(continue_),
                },
            );

            let dispatcher = Dispatcher::SucceedOrContinue {
                checkpoint: some_ident(),
                next,
                terminator: parse_quote!(|c| c.is_alphabetic()),
                body: parse_quote!(succeed),
            };
            assert_eq!(
                r#"match something . peek () { Some ('a') => { let _ = something . next () ; something . abort () } , Some ('b') => { let _ = something . next () ; if something . head_matches ("test") { if let Some (__pratt_internal_c) = something . peek () { if (char :: is_whitespace) (__pratt_internal_c) { something . commit () ; continue_ } else { something . abort () } } else { something . commit () ; continue_ } } else { something . abort () } } , Some (__pratt_internal_c) => if (| c | c . is_alphabetic ()) (__pratt_internal_c) { something . commit () ; succeed } else { something . abort () } None => { succeed } }"#,
                dispatcher.into_token_stream().to_string()
            );
        }

        #[test]
        fn continue_() {
            let mut next = BTreeMap::new();
            let _ = next.insert('a', Dispatcher::AlwaysFail(some_ident()));
            let _ = next.insert(
                'b',
                Dispatcher::TestThenSucceed {
                    checkpoint: some_ident(),
                    test: "test".chars().collect(),
                    terminator: parse_quote!(char::is_whitespace),
                    body: parse_quote!(continue_),
                },
            );

            let dispatcher = Dispatcher::Continue(some_ident(), next);
            assert_eq!(
                r#"match something . peek () { Some ('a') => { let _ = something . next () ; something . abort () } , Some ('b') => { let _ = something . next () ; if something . head_matches ("test") { if let Some (__pratt_internal_c) = something . peek () { if (char :: is_whitespace) (__pratt_internal_c) { something . commit () ; continue_ } else { something . abort () } } else { something . commit () ; continue_ } } else { something . abort () } } , _ => something . abort () }"#,
                dispatcher.into_token_stream().to_string()
            );
        }
    }
}
