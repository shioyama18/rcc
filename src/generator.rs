use lexer::*;
use parser::*;

pub fn generate(ast: &Program) -> String {
    let mut output = String::new();

    match ast {
        Program::Program(FunctionDeclaration::Function(_name, statements)) => {
            for s in statements {
                output.push_str(&generate_statement(s))
            }

            output.push_str("  pop rax\n");
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
        Expression::Constant(n) => format!("  push {}\n", n),
        Expression::UnaryOp(op, expr) => {
            let mut generated = generate_expression(expr);
            generated.push_str("  pop rax\n");

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
            generated.push_str("  push rax\n");
            return generated;
        }
        Expression::BinaryOp(op, lhs, rhs) => {
            let mut generated = generate_expression(lhs);
            generated.push_str(&generate_expression(rhs));
            generated.push_str("  pop rdi\n");
            generated.push_str("  pop rax\n");

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

            generated.push_str("  push rax\n");
            return generated;
        }
    }
}


