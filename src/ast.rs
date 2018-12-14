use token::*;

#[derive(Debug)]
pub enum Program {
    Program(FunctionDeclaration),
}
#[derive(Debug)]
pub enum FunctionDeclaration {
    Function(String, Block),
}

pub type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Option<Expression>),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Block),
    For(Option<Expression>, Expression, Option<Expression>, Box<Statement>),
    ForDeclaration(Declaration, Expression, Option<Expression>, Box<Statement>),
    While(Expression, Box<Statement>),
    Do(Box<Statement>, Expression),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum Declaration {
    Declare(String, Option<Expression>),
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
    Variable(String),
    UnaryOp(Operator, Box<Expression>),
    BinaryOp(Operator, Box<Expression>, Box<Expression>),
    AssignOp(Operator, String, Box<Expression>),
    TernaryOp(Box<Expression>, Box<Expression>, Box<Expression>),
}
