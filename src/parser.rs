use std::slice::Iter;
use peek_nth::{PeekableNth, IteratorExt};

use lexer::*;

#[derive(Debug)]
pub enum Program {
    Program(FunctionDeclaration),
}
#[derive(Debug)]
pub enum FunctionDeclaration {
    Function(String, Vec<Statement>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    UnaryOp(Operator, Box<Expression>),
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
                                                            return Program::Program(FunctionDeclaration::Function(id.to_string(), statements));
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
            statement = Statement::Return(parse_expression(tokens));
        }
        _ => panic!("Expected return statement"),
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => return statement,
        _ => panic!("Expected semicolon"),
    }
}

fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.peek_nth(0) {
        Some(Token::Constant(c)) => {
            tokens.next();
            return Expression::Constant(*c);
        }
        Some(Token::Operator(op)) => {
            tokens.next();
            return Expression::UnaryOp(*op, Box::new(parse_expression(tokens)));
        }
        _ => panic!("Expected constant"),
    }
}
