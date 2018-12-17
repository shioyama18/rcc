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
    Comma,
}

// TODO: Implement the operators shown below
// AssignBitShl: <<=
// AssignBitShr: >>=
// AssignBitAnd: &=
// AssignBitOr:  |=
// AssingBitXor: ^=
// Increment: ++
// Decrement: --
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,               // +
    Minus,              // -
    Multiplication,     // *
    Division,           // /
    Modulo,             // %
    BitwiseComplement,  // ~
    BitwiseShiftLeft,   // <<
    BitwiseShiftRight,  // >>
    BitwiseAnd,         // &
    BitwiseOr,          // |
    BitwiseXor,         // ^
    LogicalNegation,    // !
    LogicalAnd,         // &&
    LogicalOr,          // ||
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    LessThanOrEqual,    // <=
    GreaterThan,        // >
    GreaterThanOrEqual, // >=
    Assignment,         // =
    AssignPlus,         // +=
    AssignMinus,        // -=
    AssignMult,         // *=
    AssignDiv,          // /=
    AssignMod,          // %=
}

impl Operator {
    pub fn is_unary(self) -> bool {
        match self {
            Operator::Minus | Operator::LogicalNegation | Operator::BitwiseComplement => true,
            _ => false,
        }
    }

    pub fn is_assignment(self) -> bool {
        match self {
            Operator::Assignment
            | Operator::AssignPlus
            | Operator::AssignMinus
            | Operator::AssignMult
            | Operator::AssignDiv
            | Operator::AssignMod => true,
            _ => false,
        }
    }

    pub fn is_bitwise(self) -> bool {
        match self {
            Operator::BitwiseShiftLeft
            | Operator::BitwiseShiftRight
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseXor => true,
            _ => false,
        }
    }
}
