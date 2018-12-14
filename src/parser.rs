use std::slice::Iter;
use peek_nth::{PeekableNth, IteratorExt};

use token::*;
use ast::*;

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
                                            let block = parse_block(tokens);
                                            return Program::Program(FunctionDeclaration::Function(id.clone(), block));
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

fn parse_block(tokens: &mut PeekableNth<Iter<Token>>) -> Block {
    let mut block = Vec::new();
    
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenBrace)) => {
            while tokens.peek() != Some(&&Token::Punctuation(Punctuation::CloseBrace)) {
                block.push(parse_block_item(tokens));
            }
        }
        _ => panic!("Expected opening braces at start of block"),
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::CloseBrace)) => return block,
        _ => panic!("Expected closing braces at end of block: {:?}", tokens),
    }
}

fn parse_block_item(tokens: &mut PeekableNth<Iter<Token>>) -> BlockItem {
    let block_item: BlockItem;

    match tokens.peek() {
        Some(Token::Keyword(Keyword::Int)) => block_item = BlockItem::Declaration(parse_declaration(tokens)),
        Some(_) => block_item = BlockItem::Statement(parse_statement(tokens)),
        None => panic!("Expected block"),
    }

    return block_item;
}

fn parse_declaration(tokens: &mut PeekableNth<Iter<Token>>) -> Declaration {
    let declaration: Declaration;

    match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => {
            match tokens.next() {
                Some(Token::Identifier(id)) => {
                    if let Some(&&Token::Operator(Operator::Assignment)) = tokens.peek() {
                        tokens.next();
                        declaration = Declaration::Declare(id.clone(), Some(parse_expression(tokens)));
                    } else {
                        declaration = Declaration::Declare(id.clone(), None);
                    }
                }
                _ => panic!("Expected identifier"),
            }
        }
        _ => panic!("Expected int keyword"),
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => return declaration,
        _ => panic!("Expected semicolon at the end of declaration: {:?}", tokens),
    }
}

fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    let statement: Statement;

    match tokens.peek() {
        Some(Token::Keyword(Keyword::Return)) => {
            tokens.next();
            statement = Statement::Return(parse_expression(tokens));
        }
        Some(Token::Keyword(Keyword::If)) => {
            tokens.next();
            return parse_if_statement(tokens);
        } 
        Some(Token::Punctuation(Punctuation::OpenBrace)) => {
            // TODO: tokens.next() and handle body in parse_block
            let block = parse_block(tokens);
            return Statement::Compound(block);
        }
        Some(Token::Keyword(Keyword::For)) => {
            tokens.next();
            return parse_for_statement(tokens);
        }
        Some(Token::Keyword(Keyword::While)) => {
            tokens.next();
            return parse_while_statement(tokens);
        }
        Some(Token::Keyword(Keyword::Do)) => {
            tokens.next();
            statement = parse_do_statement(tokens);
        }
        Some(Token::Keyword(Keyword::Break)) => {
            tokens.next();
            statement = Statement::Break;
        }
        Some(Token::Keyword(Keyword::Continue)) => {
            tokens.next();
            statement = Statement::Continue;
        }
        _ => {
            statement = Statement::Expression(parse_optional_expression(tokens));
        }
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => return statement,
        _ => panic!("Expected semicolon at the end of statement: {:?}", tokens),
    }
}

fn parse_if_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            let controlling_condition = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::CloseParen)) => {
                    let next_statement = parse_statement(tokens);
                    match tokens.peek() {
                        Some(Token::Keyword(Keyword::Else)) => {
                            tokens.next();
                            let else_statement = parse_statement(tokens);
                            return Statement::Conditional(controlling_condition, Box::new(next_statement), Some(Box::new(else_statement)));
                        }
                        _ => return Statement::Conditional(controlling_condition, Box::new(next_statement), None),
                    }
                }
                _ => panic!("Expected closing parenthesis"),
            }
        }
        _ => panic!("Expected opening parenthesis"),
    }
}

fn parse_for_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            match tokens.peek() {
                Some(Token::Keyword(Keyword::Int)) => {
                    let init = parse_declaration(tokens);

                    let (condition, modifier, body) = parse_for_components(tokens);

                    return Statement::ForDeclaration(init, condition, modifier, Box::new(body));
                }
                _ => {
                    let init = parse_optional_expression(tokens);
                    if let Some(Token::Punctuation(Punctuation::Semicolon)) = tokens.peek() {
                        tokens.next();
                    } else {
                        panic!("Expected semicolon after initializer");
                    }

                    let (condition, modifier, body) = parse_for_components(tokens);
                    return Statement::For(init, condition, modifier, Box::new(body));
                }
            }
        }
        _ => panic!("Expected open parenthesis"),
    }
}

fn parse_for_components(tokens: &mut PeekableNth<Iter<Token>>) -> (Expression, Option<Expression>, Statement) {
    let condition = match parse_optional_expression(tokens) {
        Some(expr) => expr,
        None => Expression::Constant(1),
    };

    let modifier: Option<Expression>;
    let body: Statement;
    
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => {
            modifier = parse_optional_expression(tokens);
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::CloseParen)) => {
                    body = parse_statement(tokens);
                }
                _ => panic!("Expected close parenthesis"),
            }
        }
        _ => panic!("Expected semicolon after conditional expression"),
    }

    return (condition, modifier, body);
}

fn parse_while_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            let expr = parse_expression(tokens);
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::CloseParen)) => {
                    let body = parse_statement(tokens);
                    return Statement::While(expr, Box::new(body));
                }
                _ => panic!("Expected close parenthesis"),
            }
        }
        _ => panic!("Expected open parenthesis"),
    }
}
fn parse_do_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    let statement = parse_statement(tokens);
    match tokens.next() {
        Some(Token::Keyword(Keyword::While)) => {
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::OpenParen)) => {
                    let expr = parse_expression(tokens);
                    match tokens.next() {
                        Some(Token::Punctuation(Punctuation::CloseParen)) => {
                            return Statement::Do(Box::new(statement), expr);
                        }
                        _ => panic!("Expected close parenthesis"),
                    }
                }
                _ => panic!("Expected open parenthesis"),
            }
        }
        _ => panic!("Expected while keyword"),
    }
}

fn parse_optional_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Option<Expression> {
    match tokens.peek() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => {
            return None;
        }
        _ => {
            return Some(parse_expression(tokens));
        }
    }
}

fn parse_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    match tokens.peek() {
        Some(Token::Identifier(id)) => {
            match tokens.peek_nth(1) {
                Some(Token::Operator(op)) if op == &Operator::Assignment => {
                    tokens.next(); // id
                    tokens.next(); // =
                    return Expression::AssignOp(*op, id.clone(), Box::new(parse_expression(tokens)));
                }
                _ => parse_conditional_expression(tokens),
            }
        }
        _ => parse_conditional_expression(tokens),
    }
}

fn parse_conditional_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut expression = parse_logical_or_expression(tokens);
    
    loop {
        match tokens.peek() {
            Some(Token::Punctuation(Punctuation::QuestionMark)) => {
                tokens.next();
                let true_expression = parse_expression(tokens);
                match tokens.next() {
                    Some(Token::Punctuation(Punctuation::Colon)) => {
                        let false_expression = parse_expression(tokens);
                        expression = Expression::TernaryOp(Box::new(expression), Box::new(true_expression), Box::new(false_expression));
                    }
                    _ => panic!("Expected colon"),
                }
            }
            _ => break,
        }
    }

    return expression;
}

fn parse_logical_or_expression(tokens: &mut PeekableNth<Iter<Token>>) -> Expression {
    let mut expression = parse_logical_and_expression(tokens);
    
    loop {
        match tokens.peek() {
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
        match tokens.peek() {
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
        match tokens.peek() {
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
        match tokens.peek() {
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
        match tokens.peek() {
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
        match tokens.peek() {
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
