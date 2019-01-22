use peek_nth::{IteratorExt, PeekableNth};
use std::collections::HashMap;
use std::slice::Iter;

use crate::ast::*;
use crate::token::*;

type FunctionMap = HashMap<String, (usize, bool)>;

pub fn parse(tokens: &[Token]) -> Program {
    let mut function_map = FunctionMap::new();

    let functions = parse_functions(&mut tokens.iter().peekable_nth(), &mut function_map);

    Program::Program(functions)
}

fn parse_functions(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &mut FunctionMap,
) -> Vec<FunctionDeclaration> {
    let mut fun_decl = Vec::new();

    while let Some(_) = tokens.peek() {
        let f = parse_function(tokens, function_map);
        fun_decl.push(f);
    }

    fun_decl
}

fn parse_function(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &mut FunctionMap,
) -> FunctionDeclaration {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => match tokens.next() {
            Some(Token::Identifier(id)) => match tokens.next() {
                Some(Token::Punctuation(Punctuation::OpenParen)) => {
                    let params = parse_function_parameters(tokens);
                    let nparams = params.len();
                    let has_body =
                        tokens.peek() == Some(&&Token::Punctuation(Punctuation::OpenBrace));

                    if let Some(&(orig_nparams, orig_has_body)) = function_map.get(id) {
                        if orig_nparams != nparams {
                            panic!("Number of parameters in function conflicts with earlier declaration");
                        } else if orig_has_body && has_body {
                            panic!("Redefinition of function");
                        } else {
                            function_map.insert(id.clone(), (nparams, has_body));
                        }
                    } else {
                        function_map.insert(id.clone(), (nparams, has_body));
                    }

                    let body = match tokens.next() {
                        Some(Token::Punctuation(Punctuation::OpenBrace)) => {
                            Some(parse_block(tokens, function_map))
                        }
                        Some(Token::Punctuation(Punctuation::Semicolon)) => None,
                        _ => panic!("Unexpected token after function declaration"),
                    };

                    FunctionDeclaration::Function(id.clone(), params, body)
                }
                e => panic!("Expected opening parenthesis at {:?}", e),
            },
            _ => panic!("Expected name for function"),
        },
        _ => panic!("Expected type for function"),
    }
}

fn parse_function_parameters(tokens: &mut PeekableNth<Iter<Token>>) -> Vec<String> {
    let mut params = Vec::new();

    match tokens.peek() {
        Some(Token::Punctuation(Punctuation::CloseParen)) => {
            tokens.next();
            return params;
        }
        Some(_) => {
            let param = parse_next_parameter(tokens);
            params.push(param);

            loop {
                match tokens.next() {
                    Some(Token::Punctuation(Punctuation::CloseParen)) => break,
                    Some(Token::Punctuation(Punctuation::Comma)) => {
                        let param = parse_next_parameter(tokens);
                        params.push(param);
                    }
                    _ => panic!("Unexpected token in function parameter"),
                }
            }
        }
        None => panic!("Expected closing parenthesis"),
    }

    params
}

fn parse_next_parameter(tokens: &mut PeekableNth<Iter<Token>>) -> String {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => match tokens.next() {
            Some(Token::Identifier(id)) => id.clone(),
            _ => panic!("Expected identifier for function parameter"),
        },
        _ => panic!("Expected int keyword for function parameter"),
    }
}

fn parse_block(tokens: &mut PeekableNth<Iter<Token>>, function_map: &FunctionMap) -> Block {
    let mut block = Vec::new();

    while tokens.peek() != Some(&&Token::Punctuation(Punctuation::CloseBrace)) {
        block.push(parse_block_item(tokens, function_map));
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::CloseBrace)) => block,
        _ => panic!("Expected closing braces at end of block: {:?}", tokens),
    }
}

fn parse_block_item(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> BlockItem {
    let block_item: BlockItem;

    match tokens.peek() {
        Some(Token::Keyword(Keyword::Int)) => {
            block_item = BlockItem::Declaration(parse_declaration(tokens, function_map))
        }
        Some(_) => block_item = BlockItem::Statement(parse_statement(tokens, function_map)),
        None => panic!("Expected block"),
    }

    block_item
}

fn parse_declaration(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Declaration {
    let declaration: Declaration;

    match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => match tokens.next() {
            Some(Token::Identifier(id)) => {
                if let Some(&&Token::Operator(Operator::Assignment)) = tokens.peek() {
                    tokens.next();
                    declaration = Declaration::Declare(
                        id.clone(),
                        Some(parse_expression(tokens, function_map)),
                    );
                } else {
                    declaration = Declaration::Declare(id.clone(), None);
                }
            }
            _ => panic!("Expected identifier"),
        },
        _ => panic!("Expected int keyword"),
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => declaration,
        _ => panic!("Expected semicolon at the end of declaration: {:?}", tokens),
    }
}

fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>, function_map: &FunctionMap) -> Statement {
    let statement: Statement;

    match tokens.peek() {
        Some(Token::Keyword(Keyword::Return)) => {
            tokens.next();
            statement = Statement::Return(parse_expression(tokens, function_map));
        }
        Some(Token::Keyword(Keyword::If)) => {
            tokens.next();
            return parse_if_statement(tokens, function_map);
        }
        Some(Token::Punctuation(Punctuation::OpenBrace)) => {
            tokens.next();
            return Statement::Compound(parse_block(tokens, function_map));
        }
        Some(Token::Keyword(Keyword::For)) => {
            tokens.next();
            return parse_for_statement(tokens, function_map);
        }
        Some(Token::Keyword(Keyword::While)) => {
            tokens.next();
            return parse_while_statement(tokens, function_map);
        }
        Some(Token::Keyword(Keyword::Do)) => {
            tokens.next();
            statement = parse_do_statement(tokens, function_map);
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
            statement = Statement::Expression(parse_optional_expression(
                tokens,
                Punctuation::Semicolon,
                function_map,
            ));
        }
    }

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => statement,
        _ => panic!("Expected semicolon at the end of statement: {:?}", tokens),
    }
}

fn parse_if_statement(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Statement {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            let controlling_condition = parse_expression(tokens, function_map);
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::CloseParen)) => {
                    let next_statement = parse_statement(tokens, function_map);
                    match tokens.peek() {
                        Some(Token::Keyword(Keyword::Else)) => {
                            tokens.next();
                            let else_statement = parse_statement(tokens, function_map);
                            Statement::Conditional(
                                controlling_condition,
                                Box::new(next_statement),
                                Some(Box::new(else_statement)),
                            )
                        }
                        _ => Statement::Conditional(
                            controlling_condition,
                            Box::new(next_statement),
                            None,
                        ),
                    }
                }
                _ => panic!("Expected closing parenthesis"),
            }
        }
        _ => panic!("Expected opening parenthesis"),
    }
}

fn parse_for_statement(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Statement {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => match tokens.peek() {
            Some(Token::Keyword(Keyword::Int)) => {
                let init = parse_declaration(tokens, function_map);
                let (condition, modifier, body) = parse_for_components(tokens, function_map);
                Statement::ForDeclaration(init, condition, modifier, Box::new(body))
            }
            _ => {
                let init = parse_optional_expression(tokens, Punctuation::Semicolon, function_map);

                if let Some(Token::Punctuation(Punctuation::Semicolon)) = tokens.peek() {
                    tokens.next();
                } else {
                    panic!("Expected semicolon after initializer");
                }

                let (condition, modifier, body) = parse_for_components(tokens, function_map);
                Statement::For(init, condition, modifier, Box::new(body))
            }
        },
        _ => panic!("Expected open parenthesis"),
    }
}

fn parse_for_components(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> (Expression, Option<Expression>, Statement) {
    let condition = match parse_optional_expression(tokens, Punctuation::Semicolon, function_map) {
        Some(expr) => expr,
        None => Expression::Constant(1),
    };

    let modifier: Option<Expression>;
    let body: Statement;

    match tokens.next() {
        Some(Token::Punctuation(Punctuation::Semicolon)) => {
            modifier = parse_optional_expression(tokens, Punctuation::CloseParen, function_map);
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::CloseParen)) => {
                    body = parse_statement(tokens, function_map);
                }
                _ => panic!("Expected close parenthesis"),
            }
        }
        _ => panic!("Expected semicolon after conditional expression"),
    }

    (condition, modifier, body)
}

fn parse_while_statement(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Statement {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            let expr = parse_expression(tokens, function_map);
            match tokens.next() {
                Some(Token::Punctuation(Punctuation::CloseParen)) => {
                    let body = parse_statement(tokens, function_map);
                    Statement::While(expr, Box::new(body))
                }
                _ => panic!("Expected close parenthesis"),
            }
        }
        _ => panic!("Expected open parenthesis"),
    }
}
fn parse_do_statement(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Statement {
    let statement = parse_statement(tokens, function_map);
    match tokens.next() {
        Some(Token::Keyword(Keyword::While)) => match tokens.next() {
            Some(Token::Punctuation(Punctuation::OpenParen)) => {
                let expr = parse_expression(tokens, function_map);
                match tokens.next() {
                    Some(Token::Punctuation(Punctuation::CloseParen)) => {
                        Statement::DoWhile(Box::new(statement), expr)
                    }
                    _ => panic!("Expected close parenthesis"),
                }
            }
            _ => panic!("Expected open parenthesis"),
        },
        _ => panic!("Expected while keyword"),
    }
}

fn parse_optional_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    expected: Punctuation,
    function_map: &FunctionMap,
) -> Option<Expression> {
    match tokens.peek() {
        Some(Token::Punctuation(t)) if t == &expected => None,
        _ => Some(parse_expression(tokens, function_map)),
    }
}

fn parse_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    match tokens.peek() {
        Some(Token::Identifier(id)) => {
            match tokens.peek_nth(1) {
                Some(Token::Operator(op)) if op.is_assignment() => {
                    tokens.next(); // consume id
                    tokens.next(); // consume op
                    Expression::AssignOp(
                        *op,
                        id.clone(),
                        Box::new(parse_expression(tokens, function_map)),
                    )
                }
                _ => parse_conditional_expression(tokens, function_map),
            }
        }
        _ => parse_conditional_expression(tokens, function_map),
    }
}

fn parse_conditional_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut expression = parse_logical_or_expression(tokens, function_map);

    while let Some(Token::Punctuation(Punctuation::QuestionMark)) = tokens.peek() {
        tokens.next();
        let true_expression = parse_expression(tokens, function_map);
        match tokens.next() {
            Some(Token::Punctuation(Punctuation::Colon)) => {
                let false_expression = parse_expression(tokens, function_map);
                expression = Expression::TernaryOp(
                    Box::new(expression),
                    Box::new(true_expression),
                    Box::new(false_expression),
                );
            }
            _ => panic!("Expected colon"),
        }
    }

    expression
}

fn parse_logical_or_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut expression = parse_logical_and_expression(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op)) if op == &Operator::LogicalOr => {
                tokens.next();
                let next_expression = parse_logical_and_expression(tokens, function_map);
                expression =
                    Expression::BinaryOp(*op, Box::new(expression), Box::new(next_expression));
            }
            _ => break,
        }
    }

    expression
}

fn parse_logical_and_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut expression = parse_equality_expression(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op)) if op == &Operator::LogicalAnd => {
                tokens.next();
                let next_expression = parse_equality_expression(tokens, function_map);
                expression =
                    Expression::BinaryOp(*op, Box::new(expression), Box::new(next_expression));
            }
            _ => break,
        }
    }

    expression
}

fn parse_equality_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut term = parse_relational_expression(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op)) if op == &Operator::Equal || op == &Operator::NotEqual => {
                tokens.next();
                let next_term = parse_relational_expression(tokens, function_map);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }

    term
}

fn parse_relational_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut term = parse_bitwise_expression(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op))
                if op == &Operator::LessThan
                    || op == &Operator::LessThanOrEqual
                    || op == &Operator::GreaterThan
                    || op == &Operator::GreaterThanOrEqual =>
            {
                tokens.next();
                let next_term = parse_bitwise_expression(tokens, function_map);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }

    term
}

fn parse_bitwise_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut term = parse_additive_expression(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op)) if op.is_bitwise() => {
                tokens.next();
                let next_term = parse_additive_expression(tokens, function_map);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }

    term
}

fn parse_additive_expression(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Expression {
    let mut term = parse_term(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op)) if op == &Operator::Plus || op == &Operator::Minus => {
                tokens.next();
                let next_term = parse_term(tokens, function_map);
                term = Expression::BinaryOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break,
        }
    }

    term
}

fn parse_term(tokens: &mut PeekableNth<Iter<Token>>, function_map: &FunctionMap) -> Expression {
    let mut factor = parse_factor(tokens, function_map);

    loop {
        match tokens.peek() {
            Some(Token::Operator(op))
                if op == &Operator::Multiplication
                    || op == &Operator::Division
                    || op == &Operator::Modulo =>
            {
                tokens.next();
                let next_factor = parse_factor(tokens, function_map);
                factor = Expression::BinaryOp(*op, Box::new(factor), Box::new(next_factor));
            }
            _ => break,
        }
    }

    factor
}

fn parse_factor(tokens: &mut PeekableNth<Iter<Token>>, function_map: &FunctionMap) -> Expression {
    match tokens.next() {
        Some(Token::Punctuation(Punctuation::OpenParen)) => {
            let expression = parse_expression(tokens, function_map);
            if let Some(Token::Punctuation(Punctuation::CloseParen)) = tokens.next() {
                return expression;
            } else {
                panic!("Expected closing parenthesis");
            }
        }
        Some(Token::Operator(op)) if op.is_unary() => {
            let factor = parse_factor(tokens, function_map);
            Expression::UnaryOp(*op, Box::new(factor))
        }
        Some(Token::Constant(c)) => Expression::Constant(*c),
        Some(Token::Identifier(id)) => match tokens.peek() {
            Some(Token::Punctuation(Punctuation::OpenParen)) => {
                tokens.next();
                let args = parse_function_call(tokens, function_map);

                if let Some(&(expected_nargs, _)) = function_map.get(id) {
                    if args.len() == expected_nargs {
                        Expression::FunctionCall(id.clone(), args)
                    } else {
                        panic!("Wrong number of arguments");
                    }
                } else {
                    panic!("Undeclared function: {}", id);
                }
            }
            _ => Expression::Variable(id.clone()),
        },
        _ => panic!("Unexpected token"),
    }
}

fn parse_function_call(
    tokens: &mut PeekableNth<Iter<Token>>,
    function_map: &FunctionMap,
) -> Vec<Expression> {
    let mut args = Vec::new();

    match tokens.peek() {
        Some(Token::Punctuation(Punctuation::CloseParen)) => {
            tokens.next();
            return args;
        }
        Some(_) => {
            let arg = parse_expression(tokens, function_map);
            args.push(arg);

            loop {
                match tokens.next() {
                    Some(Token::Punctuation(Punctuation::CloseParen)) => break,
                    Some(Token::Punctuation(Punctuation::Comma)) => {
                        let arg = parse_expression(tokens, function_map);
                        args.push(arg);
                    }
                    _ => panic!("Unexpected token in function argument"),
                }
            }
        }
        None => panic!("Expected closing parenthesis"),
    }
    args
}
