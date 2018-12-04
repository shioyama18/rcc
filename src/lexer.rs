#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Constant(i32),
    Punctuation(Punctuation),
    Operator(Operator),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Int,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,                 // +
    Minus,                // -
    Multiplication,       // *
    Division,             // /
    BitwiseComplement,    // ~
    LogicalNegation,      // !
    LogicalAnd,           // &&
    LogicalOr,            // ||
    Equal,                // ==
    NotEqual,             // !=
    LessThan,             // <
    LessThanOrEqual,      // <=
    GreaterThan,          // >
    GreaterThanOrEqual,   // >=
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            | Operator::Minus
            | Operator::BitwiseComplement
            | Operator::LogicalNegation => true,
            _ => false,
        }
    }
}

use self::Operator::*;
use self::Keyword::*;
use self::Punctuation::*;

pub fn lex(input: &str) -> Vec<Token> {
    let input = input.chars().collect::<Vec<_>>();
    let mut i = 0;
    let length = input.len();
    let mut tokens = Vec::new();

    while i < length {
        match input[i] {
            '(' => tokens.push(Token::Punctuation(OpenParen)),
            ')' => tokens.push(Token::Punctuation(CloseParen)),
            '{' => tokens.push(Token::Punctuation(OpenBrace)),
            '}' => tokens.push(Token::Punctuation(CloseBrace)),
            ';' => tokens.push(Token::Punctuation(Semicolon)),
            '+' => tokens.push(Token::Operator(Plus)),
            '-' => tokens.push(Token::Operator(Minus)),
            '*' => tokens.push(Token::Operator(Multiplication)),
            '/' => tokens.push(Token::Operator(Division)),
            '!' => {
                if input.get(i+1) == Some(&'=') {
                    i += 1;
                    tokens.push(Token::Operator(NotEqual));
                } else {
                    tokens.push(Token::Operator(LogicalNegation));
                }
            }
            '~' => tokens.push(Token::Operator(BitwiseComplement)),
            '&' => {
                if input.get(i+1) == Some(&'&') {
                    i += 1;
                    tokens.push(Token::Operator(LogicalAnd));
                } else {
                    panic!("Bitwise AND not implemented yet");
                }
            }
            '|' => {
                if input.get(i+1) == Some(&'|') {
                    i += 1;
                    tokens.push(Token::Operator(LogicalOr));
                } else {
                    panic!("Bitwise Or not implemented yet");
                }
            }
            '=' => {
                if input.get(i+1) == Some(&'=') {
                    i += 1;
                    tokens.push(Token::Operator(Equal));
                } else {
                    panic!("Assignment not implemented yet");
                }
            }
            '<' => {
                if input.get(i+1) == Some(&'=') {
                    i += 1;
                    tokens.push(Token::Operator(LessThanOrEqual));
                } else {
                    tokens.push(Token::Operator(LessThan));
                }
            }
            '>' => {
                if input.get(i+1) == Some(&'=') {
                    i += 1;
                    tokens.push(Token::Operator(GreaterThanOrEqual));
                } else {
                    tokens.push(Token::Operator(GreaterThan));
                }
            }
            c => {
                if c.is_alphabetic() {
                    let mut s = c.to_string();
                    i += 1;
                    
                    while i < length && input[i].is_alphanumeric() {
                        s.push(input[i]);
                        i += 1;
                    }
                    
                    match &s[..] {
                        "int" => tokens.push(Token::Keyword(Int)),
                        "return" => tokens.push(Token::Keyword(Return)),
                        _ => tokens.push(Token::Identifier(s)),
                    }
                    
                    continue;
                } else if c.is_digit(10) {
                    let mut n = c.to_string();
                    i += 1;

                    while i < length && input[i].is_digit(10) {
                        n.push(input[i]);
                        i += 1;
                    }

                    let n = n.parse::<i32>().unwrap();
                    tokens.push(Token::Constant(n));

                    continue;
                }
            }
        }

        i += 1;
    }

    tokens
}
