#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Constant(i32),
    Punctuation(Punctuation),
    Operator(Operator)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Int,
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Colon,
    QuestionMark,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,                 // +
    Minus,                // -
    Multiplication,       // *
    Division,             // /
    Modulo,               // %
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
    Assignment,           // =
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            | Operator::Minus
            | Operator::LogicalNegation
            | Operator::BitwiseComplement => true,
            _ => false,
        }
    }
    
    pub fn is_assignment(&self) -> bool {
        match self {
            Operator::Assignment => true,
            _ => false,
        }
    }
}
