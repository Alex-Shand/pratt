```
# // Pratt doctest/example
# use pratt::Token as _;
use std::iter::Peekable;

use pratt::Result;
use span::Span;

#[derive(Debug, pratt::Token)]
#[pratt(crate = pratt)]
enum Token {
    #[pratt(payload = *it.0)]
    Atom(char),
    #[pratt(payload = '+')]
    Add,
    #[pratt(payload = '-')]
    Sub,
    #[pratt(payload = '*')]
    Mul,
    #[pratt(payload = '/')]
    Div
}

#[derive(Debug, PartialEq)]
enum Ast {
    BinOp(char, Box<Ast>, Box<Ast>),
    Atom(char)
}

fn atom(
    _: &pratt::Table<Token, (), Ast>,
    tokens: &mut dyn pratt::Lexer<Token = Token, Context = ()>,
    (): (),
) -> Result<Token, Ast> {
    let Some(Token::Atom(c)) = tokens.token(()) else {
        unreachable!();
    };
    Ok(Ast::Atom(c))
}

fn op(
    table: &pratt::Table<Token, (), Ast>,
    tokens: &mut dyn pratt::Lexer<Token = Token, Context = ()>,
    (): (),
    lhs: Ast
) -> Result<Token, Ast> {
    let Some(tok) = tokens.token(()) else {
        unreachable!();
    };
    let op = tok.payload().chars().next().unwrap();
    let bind = table.bind_of(&tok.typ());
    let rhs = table.parse_at(tokens, (), bind)?;
    Ok(Ast::BinOp(op, Box::new(lhs), Box::new(rhs)))
}

let table = pratt::pratt! {
    Add  => [     , op, L, PLUS  ];
    Sub  => [     , op, L, =PLUS ];
    Mul  => [     , op, L, MUL   ];
    Div  => [     , op, L, =MUL  ];
    Atom => [ atom,   , _,     _ ];
};

let mut tokens = {
    use Token::*;
    //         a     +         b     -         c     *         d     /         e
    vec![Atom('a'), Add, Atom('b'), Sub, Atom('c'), Mul, Atom('d'), Div, Atom('e')]
        .into_iter()
};
let tokens: &mut dyn Iterator<Item=Token> = &mut tokens;
let result = table.parse(&mut tokens.peekable(), ()).unwrap();
// (a + b) - ((c * d) / e)
let expected = Ast::BinOp(
    '-',
    Box::new(Ast::BinOp(
        '+',
        Box::new(Ast::Atom('a')),
        Box::new(Ast::Atom('b'))
    )),
    Box::new(Ast::BinOp(
        '/',
        Box::new(Ast::BinOp(
            '*',
            Box::new(Ast::Atom('c')),
            Box::new(Ast::Atom('d'))
        )),
        Box::new(Ast::Atom('e'))
    ))
);
assert_eq!(result, expected);
```
