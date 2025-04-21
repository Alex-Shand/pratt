```
# // Pratt doctest/example
# use pratt::Token as _;
use std::iter::Peekable;

use pratt::Result;
use span::Span;

#[derive(Debug, Copy, Clone, pratt::Token)]
#[pratt(crate = pratt)]
enum Token {
    #[pratt(payload = *it)]
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

type Table = pratt::Table<Token, (), Ast>;
type Lexer<'a> = pratt::LexerHandle<'a, Token, ()>;

#[pratt::prefix(crate = pratt)]
fn atom(
    _: &Table,
    tokens: &mut Lexer<'_>,
    (): (),
) -> Result<Token, Ast> {
    let c = demand!(Token::Atom(c) => *c);
    Ok(Ast::Atom(c))
}

#[pratt::infix(crate = pratt)]
fn op(
    table: &Table,
    tokens: &mut Lexer<'_>,
    (): (),
    lhs: Ast
) -> Result<Token, Ast> {
    let tok = demand!(tok@(Token::Add|Token::Sub|Token::Mul|Token::Div) => *tok);
    let op = tok.payload().chars().next().unwrap();
    let rhs = table.parse_at(tokens, (), tok.typ())?;
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
