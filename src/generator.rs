use lexer::*;
use parser::*;

pub fn generate(ast: &Program) -> String {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let mut output = String::new();

    match ast {
        Program::Program(FunctionDeclaration::Function(_name, statements)) => {
            for s in statements {
                output.push_str(&generate_statement(s))
            }

            output.push_str("  ret\n");
        }
    }

    output
}

fn generate_statement(statement: &Statement) -> String {
    let mut output = String::new();

    match statement {
        Statement::Return(expr) => {
            output.push_str(&generate_expression(expr));
        }
    }

    output
}

fn generate_expression(expression: &Expression) -> String {
    match expression {
        Expression::Constant(n) => format!("  mov rax, {}\n", n),
        Expression::UnaryOp(op, expr) => {
            let mut generated = generate_expression(expr);

            match op {
                Operator::Minus => {
                    generated.push_str("  neg rax\n");
                }
                Operator::BitwiseComplement => {
                    generated.push_str("  not rax\n");
                }
                Operator::LogicalNegation => {
                    generated.push_str("  mov rdi, 0\n");
                    generated.push_str("  cmp rdi, rax\n");
                    generated.push_str("  sete al\n");
                    generated.push_str("  movzb rax, al\n");
                }
                _ => panic!("Unexpected unary operator"),
            }

            return generated;
        }
        Expression::BinaryOp(op, lhs, rhs) => {
            let mut generated: String;
            match op {
                Operator::Plus | Operator::Minus | Operator::Multiplication | Operator::Division => {
                    generated = generate_expression(rhs);
                    generated.push_str("  push rax\n");
                    generated.push_str(&generate_expression(lhs));
                    generated.push_str("  pop rdi\n");

                    match op {
                        Operator::Plus => generated.push_str("  add rax, rdi\n"),
                        Operator::Minus => generated.push_str("  sub rax, rdi\n"),
                        Operator::Multiplication => generated.push_str("  mul rdi\n"),
                        Operator::Division => {
                            generated.push_str("  mov rdx, 0\n");
                            generated.push_str("  div rdi\n");
                        }
                        _ => panic!("Unexpected binary operator"),
                    }
                    return generated;
                }
                Operator::Equal | Operator::NotEqual | 
                Operator::LessThan | Operator::LessThanOrEqual | 
                Operator::GreaterThan | Operator::GreaterThanOrEqual => {
                    generated = generate_expression(rhs);
                    generated.push_str("  push rax\n");
                    generated.push_str(&generate_expression(lhs));
                    generated.push_str("  pop rdi\n");

                    generated.push_str("  cmp rax, rdi\n");
                    match op {
                        Operator::Equal => generated.push_str("  sete al\n"),
                        Operator::NotEqual => generated.push_str("  setne al\n"),
                        Operator::LessThan => generated.push_str("  setl al\n"),
                        Operator::LessThanOrEqual => generated.push_str("  setle al\n"),
                        Operator::GreaterThan => generated.push_str("  setg al\n"),
                        Operator::GreaterThanOrEqual => generated.push_str("  setge al\n"),
                        _ => panic!("Unexpected relational operator"),
                    }
                    generated.push_str("  movzb rax, al\n");
                    return generated;
                }
                Operator::LogicalOr | Operator::LogicalAnd => {
                    generated = generate_expression(rhs);
                    generated.push_str("  push rax\n");
                    generated.push_str(&generate_expression(lhs));
                    generated.push_str("  pop rdi\n");
                    
                    match op {
                        Operator::LogicalOr => {
                            generated.push_str("  or rdi, rax\n");
                            generated.push_str("  setne al\n");
                            generated.push_str("  movzb rax, al\n");
                        }
                        Operator::LogicalAnd => {
                            generated.push_str("  cmp rdi, 0\n");
                            generated.push_str("  setne dil\n");
                            generated.push_str("  cmp rax, 0\n");
                            generated.push_str("  setne al\n");
                            generated.push_str("  movzb rax, al\n");
                            generated.push_str("  and al, dil\n");
                        }
                        _ => panic!("Unexpected logical binary operator"),
                    }
                    
                    return generated;
                }
                _ => panic!("Unexpected binary operator"),
            }
        }
    }
}
