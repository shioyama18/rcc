use lexer::Operator::*;
use parser::*;

pub fn generate(ast: &Program) -> String {
    let mut output = String::new();

    match ast {
        Program::Program(FunctionDeclaration::Function(name, statements)) => {
            output.push_str(&format!("  .global {}\n", name));
            
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
                Minus => generated.push_str("  neg rax\n"),
                BitwiseComplement => generated.push_str("  not rax\n"),
                LogicalNegation => {
                    generated.push_str("  cmp 0, rax\n");
                    generated.push_str("  sete al\n");
                    generated.push_str("  movzb rax, al\n");
                }
            }
            
            return generated;
        }
    }
}
