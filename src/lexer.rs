#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Constant(i32),
    Punctuation(Punctuation),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Int,
    Return,
}

#[derive(Debug, PartialEq)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

use self::Punctuation::*;
use self::Keyword::*;

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
