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
    Assignment,           // =
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
