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
    BinaryOp(Operator, Box<Expression>, Box<Expression>),
    Constant(i32),
}

pub fn parse(tokens: &[Token]) -> Program {
    parse_main(&mut tokens.iter().peekable_nth())
}

fn parse_main(tokens: &mut PeekableNth<Iter<Token>>) -> Program {
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
                                _ => panic!("Expected opening parenthesis"),
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
        _ => panic!("Expected semicolon at the end of statement: {:?}", tokens),
    }
}

fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut term = parse_term(tokens);

    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(op)) if op == &Operator::Plus || op == &Operator::Minus => {
                tokens.next();
                let next_term = parse_term(tokens);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }
    
    term
}

fn parse_term(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut factor = parse_factor(tokens);

    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(op)) if op == &Operator::Multiplication || op == &Operator::Division => {
                tokens.next();
                let next_factor = parse_factor(tokens);
                factor = Expression::BinaryOp(*op, Box::new(factor), Box::new(next_factor));
            }
            _ => break,
        }
    }
    
    factor
}

fn parse_factor(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            let expression = parse_expression(tokens);
            if let Some(Token::Punctuation(Punctuation::CloseParen)) = tokens.next() {
                return expression;
            } else {
                panic!("Expected closing parenthesis");
            }
        }
        Some(Token::Operator(op)) if op.is_unary() => {
            let factor = parse_factor(tokens);
            return Expression::UnaryOp(*op, Box::new(factor));
        }
        Some(Token::Constant(c)) => {
            return Expression::Constant(*c);
        }
        _ => panic!("Unexpected token"),
    }
}
