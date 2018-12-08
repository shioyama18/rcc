use std::collections::HashMap;

use token::*;
use ast::*;

type VariableMap = HashMap<String, isize>;

pub fn generate(ast: &Program) -> String {
    let mut output = String::new();
    output.push_str(".intel_syntax noprefix\n");

    match ast {
        Program::Program(FunctionDeclaration::Function(name, blocks)) => {
            let mut var_map = VariableMap::new();
            let mut stack_index: isize = -8;

            output.push_str(&format!(".global {}\n", name));
            output.push_str(&format!("{}:\n", name));

            output.push_str("  push rbp\n");
            output.push_str("  mov rbp, rsp\n");

            for block in blocks {
                output.push_str(&generate_block(block, &mut var_map, &mut stack_index));
            }

            if output.ends_with(&format!("  ret\n")) {
                return output;
            } else {
                output.push_str("  mov rax, 0\n");
                generate_function_epilogue(&mut output);
            }
        }
    }

    output
}

fn generate_block(block: &BlockItem, var_map: &mut VariableMap, stack_index: &mut isize) -> String {
    let mut output = String::new();

    match block {
        BlockItem::Declaration(declaration) => output.push_str(&generate_declaration(declaration, var_map, stack_index)),
        BlockItem::Statement(statement) => output.push_str(&generate_statement(statement, var_map, stack_index)),
    }

    return output;
}

fn generate_declaration(declaration: &Declaration, var_map: &mut VariableMap, stack_index: &mut isize) -> String {
    let mut output = String::new();

    match declaration {
        Declaration::Declare(name, value) => {
            if var_map.contains_key(name) {
                panic!("Variable {} declared twice in same scope", name);
            }

            if let Some(expr) = value {
                output.push_str(&generate_expression(expr, var_map, stack_index));
                output.push_str("  push rax\n");
            } else {
                output.push_str("  push 0\n");
            }

            var_map.insert(name.clone(), *stack_index);
            *stack_index -= 8;
        }
    }

    return output;
}

fn generate_statement(statement: &Statement, var_map: &mut VariableMap, stack_index: &mut isize) -> String {
    let mut output = String::new();

    match statement {
        Statement::Return(expr) => {
            output.push_str(&generate_expression(expr, var_map, stack_index));
            generate_function_epilogue(&mut output);
        }
        Statement::Expression(expr) => {
            output.push_str(&generate_expression(expr, var_map, stack_index));
        }
        Statement::Conditional(expr, true_statement, false_statement) => {
            // call generate_if_statement
        }
        _ => unimplemented!("if not implemented yet"),
    }

    return output;
}

fn generate_if_statement(statement: &Statement, var_map: &mut VariableMap, stack_index: &mut isize) -> String {
    let mut output = String::new();

    return output;
}

fn generate_expression(expression: &Expression, var_map: &VariableMap, stack_index: &isize) -> String {
    match expression {
        Expression::Constant(n) => format!("  mov rax, {}\n", n),
        Expression::UnaryOp(op, expr) => {
            let mut generated = generate_expression(expr, var_map, stack_index);

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
            let mut generated = generate_expression(rhs, var_map, stack_index);
            generated.push_str("  push rax\n");
            generated.push_str(&generate_expression(lhs, var_map, stack_index));
            generated.push_str("  pop rdi\n");

            match op {
                Operator::Plus | Operator::Minus | Operator::Multiplication | Operator::Division => {
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
        Expression::AssignOp(_op, name, expr) => {
            // TODO: implement compound assignment operators
            let mut generated = generate_expression(expr, var_map, stack_index);
            
            if !var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *var_map.get(name).expect("Missing offset");
                generated.push_str(&format!("  mov [rbp{}], rax\n", offset));
            }

            return generated;
        }
        Expression::Variable(name) => {
            if !var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *var_map.get(name).expect("Missing offset");
                return format!("  mov rax, [rbp{}]\n", offset);
            }
        }
        Expression::TernaryOp(_, _, _) => {
            // Implement ternary operator
            return String::new();
        }
        _ => unimplemented!(),
    }
}

fn generate_function_epilogue(output: &mut String) {
    output.push_str("  mov rsp, rbp\n");
    output.push_str("  pop rbp\n");
    output.push_str("  ret\n");
}

