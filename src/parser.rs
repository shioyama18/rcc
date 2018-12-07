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
    Declare(String, Option<Expression>),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Assign(String, Box<Expression>),
    UnaryOp(Operator, Box<Expression>),
    BinaryOp(Operator, Box<Expression>, Box<Expression>),
    Constant(i32),
    Variable(String),
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
        Some(Token::Keyword(Keyword::Int)) => {
            tokens.next();
            match tokens.next() {
                Some(Token::Identifier(id)) => {
                    if let Some(&&Token::Operator(Operator::Assignment)) = tokens.peek_nth(0) {
                        tokens.next();
                        statement = Statement::Declare(id.clone(), Some(parse_expression(tokens)));
                    } else {
                        statement = Statement::Declare(id.clone(), None);
                    }
                }
                _ => panic!("Expected identifier"),
            }
        }
        None => panic!("Expected statement"),
        _ => statement = Statement::Expression(parse_expression(tokens)),
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => return statement,
        _ => panic!("Expected semicolon at the end of statement: {:?}", tokens),
    }
}

fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.peek_nth(0) {
        Some(Token::Identifier(id)) => {
            match tokens.peek_nth(1) {
                Some(Token::Operator(op)) if op == &Operator::Assignment => {
                    tokens.next(); // id
                    tokens.next(); // =
                    return Expression::Assign(id.clone(), Box::new(parse_expression(tokens)));
                }
                _ => parse_logical_or_expression(tokens),
            }
        }
        _ => parse_logical_or_expression(tokens),
    }
}

/// TODO: Make all binary operation a macro

fn parse_logical_or_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut expression = parse_logical_and_expression(tokens);
    
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(op)) if op == &Operator::LogicalOr => {
                tokens.next();
                let next_expression = parse_logical_and_expression(tokens);
                expression = Expression::BinaryOp(*op, Box::new(expression), Box::new(next_expression));
            }
            _ => break,
        }
    }

    expression
}

fn parse_logical_and_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut expression = parse_equality_expression(tokens);
    
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(op)) if op == &Operator::LogicalAnd => {
                tokens.next();
                let next_expression = parse_equality_expression(tokens);
                expression = Expression::BinaryOp(*op, Box::new(expression), Box::new(next_expression));
            }
            _ => break,
        }
    }

    expression
}

fn parse_equality_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut term = parse_relational_expression(tokens);

    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(op)) if op == &Operator::Equal || op == &Operator::NotEqual => {
                tokens.next();
                let next_term = parse_relational_expression(tokens);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }
    
    term
}

fn parse_relational_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut term = parse_additive_expression(tokens);

    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(op)) if op == &Operator::LessThan
                || op == &Operator::LessThanOrEqual
                || op == &Operator::GreaterThan
                || op == &Operator::GreaterThanOrEqual
            => {
                tokens.next();
                let next_term = parse_additive_expression(tokens);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }
    
    term
}

fn parse_additive_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
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
        Some(Token::Identifier(id)) => {
            return Expression::Variable(id.clone());
        }
        _ => panic!("Unexpected token"),
    }
}
