use std::cmp::max;

use super::{
    actions::action,
    goto::goto,
    lexer::{Token, TokenKind},
    nonterminals::{NonTerminal, TranslationUnit},
    rules::rules,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken(TokenKind),
    UnexpectedNonTerminal(NonTerminal),
    WrongParsedToReduce(usize),
    EmptyParseStack,
    EmptyStateStack,
    EmptyTokenQueue,
    ExpectedNonTerminalFoundTerminal,
    CouldntFindTranslationUnit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenOrNonTerminal {
    Token(Token),
    NonTerminal(NonTerminal),
}

pub type ParseResult<T> = Result<T, ParseError>;
pub type Production = fn(&mut Parser) -> ParseResult<(NonTerminal, usize)>;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    state_stack: Vec<usize>,
    rules: Vec<Production>,
    parse_stack: Vec<TokenOrNonTerminal>,
}

// generate_reduce_fn!(
//     NonTerminal::PrimaryExpression(PrimaryExpression::ParenthesizedExpression(expr.clone())),
//     TokenOrNonTerminal::Token(Token {
//         kind: TokenKind::Delimiter(Delimiter::LeftParen),
//         ..
//     }),
//     TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expr)),
//     TokenOrNonTerminal::Token(Token {
//         kind: TokenKind::Delimiter(Delimiter::RightParen),
//         ..
//     })
// ) // results in the following:
// |parser: &mut Parser| {
//    let last_n = parser.last_n_parse_stack(3); // counts how many elements after first expression
//    // in macro
//    match last_n.as_slice() {
//         // wraps everything in a slice
//         [TokenOrNonTerminal::Token(Token {
//             kind: TokenKind::Delimiter(Delimiter::LeftParen),
//             ..
//         }), TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expr)), TokenOrNonTerminal::Token(Token {
//             kind: TokenKind::Delimiter(Delimiter::RightParen),
//             ..
//         })] => Ok((
//             NonTerminal::PrimaryExpression(PrimaryExpression::ParenthesizedExpression(
//                 expr.clone(),
//             )),
//             3,
//         )),
//         [] => Err(ParseError::ReducingEmptyParseStack),
//         _ => Err(ParseError::WrongParsedToReduce(3)),
//     }
// }

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.to_owned(),
            pos: 0,
            state_stack: vec![0],
            rules: rules(),
            parse_stack: vec![],
        }
    }

    pub fn parse(&mut self) -> ParseResult<TranslationUnit> {
        loop {
            println!("state_stack: {:?}", self.state_stack);
            println!("parse_stack: {:#?}", self.parse_stack);
            println!("pos: {}", self.pos);
            let token_kind = self
                .tokens
                .get(self.pos)
                .map(|t| t.kind.clone())
                .unwrap_or(TokenKind::EOF);
            println!("token: {:?}", token_kind);
            let action = action(self.current_state(), &token_kind)?;
            println!("action: {:?}", action);

            match action {
                Action::Shift(s) => {
                    self.shift(s)?;
                }
                Action::Reduce(r) => {
                    self.reduce(r)?;
                }
                Action::Accept => break,
            };
        }
        self.parse_stack
            .first()
            .ok_or(ParseError::EmptyParseStack)
            .and_then(|top| match top {
                TokenOrNonTerminal::NonTerminal(NonTerminal::TranslationUnit(translation_unit)) => {
                    Ok(translation_unit.clone())
                }
                TokenOrNonTerminal::NonTerminal(_) => Err(ParseError::CouldntFindTranslationUnit),
                _ => Err(ParseError::ExpectedNonTerminalFoundTerminal),
            })
    }
    fn current_state(&self) -> usize {
        *self.state_stack.last().unwrap()
    }

    fn reduce(&mut self, index: usize) -> ParseResult<()> {
        let (reduced, to_pop) = (self.rules[index])(self)?;
        for _ in 0..to_pop {
            self.parse_stack.pop();
            self.state_stack.pop();
        }
        let goto = goto(self.current_state(), &reduced)?;
        self.parse_stack
            .push(TokenOrNonTerminal::NonTerminal(reduced));
        self.state_stack.push(goto);
        Ok(())
    }
    pub fn last_n_parse_stack(&self, n: usize) -> Vec<TokenOrNonTerminal> {
        if n > self.parse_stack.len() {
            return self.parse_stack.clone();
        }
        self.parse_stack[(self.parse_stack.len() - n)..].to_vec()
    }
    fn shift(&mut self, index: usize) -> ParseResult<()> {
        let token = self.tokens[self.pos].clone();
        self.parse_stack.push(TokenOrNonTerminal::Token(token));
        self.state_stack.push(index);
        self.pos += 1;
        Ok(())
    }
}
