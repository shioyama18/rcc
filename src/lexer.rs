use crate::token::Keyword::*;
use crate::token::Operator::*;
use crate::token::Punctuation::*;
use crate::token::*;

pub fn lex(input: &str) -> Vec<Token> {
    let mut input = input.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(c) = input.next() {
        match c {
            '(' => tokens.push(Token::Punctuation(OpenParen)),
            ')' => tokens.push(Token::Punctuation(CloseParen)),
            '{' => tokens.push(Token::Punctuation(OpenBrace)),
            '}' => tokens.push(Token::Punctuation(CloseBrace)),
            ';' => tokens.push(Token::Punctuation(Semicolon)),
            ':' => tokens.push(Token::Punctuation(Colon)),
            '?' => tokens.push(Token::Punctuation(QuestionMark)),
            '+' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(AssignPlus));
                } else {
                    tokens.push(Token::Operator(Plus));
                }
            }
            '-' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(AssignMinus));
                } else {
                    tokens.push(Token::Operator(Minus));
                }
            }
            '*' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(AssignMult));
                } else {
                    tokens.push(Token::Operator(Multiplication));
                }
            }
            '/' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(AssignDiv));
                } else {
                    tokens.push(Token::Operator(Division));
                }
            }
            '%' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(AssignMod));
                } else {
                    tokens.push(Token::Operator(Modulo));
                }
            }
            '!' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(NotEqual));
                } else {
                    tokens.push(Token::Operator(LogicalNegation));
                }
            }
            '~' => tokens.push(Token::Operator(BitwiseComplement)),
            '&' => {
                if let Some(&'&') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(LogicalAnd));
                } else {
                    tokens.push(Token::Operator(BitwiseAnd));
                }
            }
            '|' => {
                if let Some(&'|') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(LogicalOr));
                } else {
                    tokens.push(Token::Operator(BitwiseOr));
                }
            }
            '=' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(Equal));
                } else {
                    tokens.push(Token::Operator(Assignment));
                }
            }
            '<' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(LessThanOrEqual));
                } else if let Some(&'<') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(BitwiseShiftLeft));
                } else {
                    tokens.push(Token::Operator(LessThan));
                }
            }
            '>' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(GreaterThanOrEqual));
                } else if let Some(&'>') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(BitwiseShiftRight));
                } else {
                    tokens.push(Token::Operator(GreaterThan));
                }
            }
            '^' => tokens.push(Token::Operator(BitwiseXor)),
            ',' => tokens.push(Token::Punctuation(Punctuation::Comma)),
            _ => {
                if c.is_alphabetic() {
                    let mut s = c.to_string();

                    loop {
                        match input.peek() {
                            Some(&a) if a.is_alphanumeric() || a == '_' => s.push(a),
                            _ => break,
                        }
                        input.next();
                    }

                    match &s[..] {
                        "int" => tokens.push(Token::Keyword(Int)),
                        "return" => tokens.push(Token::Keyword(Return)),
                        "if" => tokens.push(Token::Keyword(If)),
                        "else" => tokens.push(Token::Keyword(Else)),
                        "for" => tokens.push(Token::Keyword(For)),
                        "while" => tokens.push(Token::Keyword(While)),
                        "do" => tokens.push(Token::Keyword(Do)),
                        "break" => tokens.push(Token::Keyword(Break)),
                        "continue" => tokens.push(Token::Keyword(Continue)),
                        _ => tokens.push(Token::Identifier(s)),
                    }
                } else if c.is_digit(10) {
                    let mut n = c.to_string();

                    loop {
                        match input.peek() {
                            Some(c) if c.is_digit(10) => n.push(*c),
                            _ => break,
                        }
                        input.next();
                    }

                    let n = n.parse::<i32>().unwrap();
                    tokens.push(Token::Constant(n));
                }
            }
        }
    }

    tokens
}
