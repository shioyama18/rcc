use parser::*;

pub fn generate(ast: &Program) -> String {
    let mut output = String::new();

    match ast {
        Program::Function(FunDecl::Fun(name, statements)) => {
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

fn generate_expression(expr: &Expr) -> String {
    match expr {
        Expr::Constant(c) => format!("  mov rax, {}\n", c),
    }
}
