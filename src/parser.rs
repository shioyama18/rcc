use std::slice::Iter;
use peek_nth::{PeekableNth, IteratorExt};

use lexer::*;

#[derive(Debug)]
pub enum Program {
    Function(FunDecl),
}
#[derive(Debug)]
pub enum FunDecl {
    Fun(String, Vec<Statement>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Constant(i32),
}

pub fn parse(tokens: &[Token]) -> Program {
    parse_function(&mut tokens.iter().peekable_nth())
}

fn parse_function(tokens: &mut PeekableNth<Iter<Token>>) -> Program {
    match tokens.next() {
        Some(token) => {
            match token {
                Token::Keyword(Keyword::Int) => {
                    match tokens.next() {
                        Some(Token::Identifier(id)) => {
                            match tokens.next() {
                                Some(Token::Punctuation(Punctuation::OpenParen)) => {
                                    match tokens.next() {
                                        Some(Token::Punctuation(Punctuation::CloseParen)) => {
                                            match tokens.next() {
                                                Some(Token::Punctuation(Punctuation::OpenBrace)) => {
                                                    let mut statements = Vec::new();
                                                    while tokens.peek_nth(0) != Some(&&Token::Punctuation(Punctuation::CloseBrace)) {
                                                        statements.push(parse_statement(tokens));
                                                    }

                                                    match tokens.next() {
                                                        Some(Token::Punctuation(Punctuation::CloseBrace)) => {
                                                            return Program::Function(FunDecl::Fun(id.to_string(), statements));
                                                        }
                                                        _ => panic!("Expected closing braces"),
                                                    }
                                                }
                                                _ => panic!("Expected opening braces"),
                                            }
                                        }
                                        _ => panic!("Expected closing parenthesis"),
                                    }
                                }
                                _ => panic!("Expected openinig parenthesis"),
                            }
                        }
                        _ => panic!("Expected name for function"),
                    }
                }
                _ => panic!("Expected type for function"),
            }
        }
        None => panic!("Expected function"),
    }
}

fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    let statement: Statement;

    match tokens.peek_nth(0) {
        Some(Token::Keyword(Keyword::Return)) => {
            tokens.next();
            statement = Statement::Return(parse_expr(tokens));
        }
        _ => panic!("Expected return statement"),
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => return statement,
        _ => panic!("Expected semicolon"),
    }
}

fn parse_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr {
    match tokens.peek_nth(0) {
        Some(Token::Constant(c)) => {
            tokens.next();
            return Expr::Constant(*c);
        }
        _ => panic!("Expected constant"),
    }
}
