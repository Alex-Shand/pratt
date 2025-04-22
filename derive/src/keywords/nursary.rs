#![allow(unreachable_pub)]

use proc::{
    syn::{
        braced, bracketed,
        parse::{Parse, ParseStream},
    },
    Result,
};

#[derive(Debug)]
pub struct Braced<T: Parse>(pub T);

impl<T: Parse> Parse for Braced<T> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let content;
        let _ = braced!(content in input);
        Ok(Braced(content.parse()?))
    }
}

#[derive(Debug)]
pub struct Bracketed<T: Parse>(pub T);

impl<T: Parse> Parse for Bracketed<T> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let content;
        let _ = bracketed!(content in input);
        Ok(Bracketed(content.parse()?))
    }
}

#[derive(Debug)]
pub struct GreedySequence<T: Parse>(pub Vec<T>);

impl<T: Parse> Parse for GreedySequence<T> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut result = Vec::new();
        while !input.is_empty() {
            result.push(input.parse()?);
        }
        Ok(Self(result))
    }
}
