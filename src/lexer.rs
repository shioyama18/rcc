use token::*;
use token::Operator::*;
use token::Keyword::*;
use token::Punctuation::*;

pub fn lex(input: &str) -> Vec<Token> {
    let mut input = input.chars().peekable();
    let mut tokens = Vec::new();

    // TODO: implement Compound Assignment Operator
    while let Some(c) = input.next() {
        match c {
            '(' => tokens.push(Token::Punctuation(OpenParen)),
            ')' => tokens.push(Token::Punctuation(CloseParen)),
            '{' => tokens.push(Token::Punctuation(OpenBrace)),
            '}' => tokens.push(Token::Punctuation(CloseBrace)),
            ';' => tokens.push(Token::Punctuation(Semicolon)),
            ':' => tokens.push(Token::Punctuation(Colon)),
            '?' => tokens.push(Token::Punctuation(QuestionMark)),
            '+' => tokens.push(Token::Operator(Plus)),
            '-' => tokens.push(Token::Operator(Minus)),
            '*' => tokens.push(Token::Operator(Multiplication)),
            '/' => tokens.push(Token::Operator(Division)),
            '%' => tokens.push(Token::Operator(Modulo)),
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
                    panic!("Bitwise AND not implemented yet");
                }
            }
            '|' => {
                if let Some(&'|') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(LogicalOr));
                } else {
                    panic!("Bitwise Or not implemented yet");
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
                } else {
                    tokens.push(Token::Operator(LessThan));
                }
            }
            '>' => {
                if let Some(&'=') = input.peek() {
                    input.next();
                    tokens.push(Token::Operator(GreaterThanOrEqual));
                } else {
                    tokens.push(Token::Operator(GreaterThan));
                }
            }
            _ => {
                if c.is_alphabetic() {
                    let mut s = c.to_string();
                    
                    loop {
                        match input.peek() {
                            Some(a) if a.is_alphanumeric() => s.push(*a),
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
