use token::*;

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
    Assign(Operator, String, Box<Expression>),
    UnaryOp(Operator, Box<Expression>),
    BinaryOp(Operator, Box<Expression>, Box<Expression>),
    Constant(i32),
    Variable(String),
}
