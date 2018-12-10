use std::collections::HashMap;

use util::*;
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
        Statement::Conditional(expr, if_body, else_body) => {
            let post_if_label = unique("post_if_");

            output.push_str(&generate_expression(expr, var_map, stack_index));
            output.push_str("  cmp rax, 0\n");
            output.push_str(&format!("  je {}\n", post_if_label));
            output.push_str(&generate_statement(if_body, var_map, stack_index));

            if let Some(else_statement) = else_body {
                let post_else_label = unique("post_else_");
                output.push_str(&format!("  jmp {}\n", post_else_label));
                
                output.push_str(&format!("{}:\n", post_if_label));
                output.push_str(&generate_statement(else_statement, var_map, stack_index));
                
                output.push_str(&format!("{}:\n", post_else_label));
            } else {
                output.push_str(&format!("{}:\n", post_if_label));
            }

            return output;
        }
    }

    return output;
}

fn generate_expression(expression: &Expression, var_map: &VariableMap, stack_index: &isize) -> String {
    match expression {
        Expression::Constant(n) => format!("  mov rax, {}\n", n),
        Expression::UnaryOp(op, expr) => {
            let mut output = generate_expression(expr, var_map, stack_index);

            match op {
                Operator::Minus => {
                    output.push_str("  neg rax\n");
                }
                Operator::BitwiseComplement => {
                    output.push_str("  not rax\n");
                }
                Operator::LogicalNegation => {
                    output.push_str("  mov rdi, 0\n");
                    output.push_str("  cmp rdi, rax\n");
                    output.push_str("  sete al\n");
                    output.push_str("  movzb rax, al\n");
                }
                _ => panic!("Unexpected unary operator"),
            }

            return output;
        }
        Expression::BinaryOp(op, lhs, rhs) => {
            let mut output = generate_expression(rhs, var_map, stack_index);
            output.push_str("  push rax\n");
            output.push_str(&generate_expression(lhs, var_map, stack_index));
            output.push_str("  pop rdi\n");

            match op {
                Operator::Plus | Operator::Minus | Operator::Multiplication | Operator::Division => {
                    match op {
                        Operator::Plus => output.push_str("  add rax, rdi\n"),
                        Operator::Minus => output.push_str("  sub rax, rdi\n"),
                        Operator::Multiplication => output.push_str("  mul rdi\n"),
                        Operator::Division => {
                            output.push_str("  mov rdx, 0\n");
                            output.push_str("  div rdi\n");
                        }
                        _ => panic!("Unexpected binary operator"),
                    }
                    return output;
                }
                Operator::Equal | Operator::NotEqual | 
                Operator::LessThan | Operator::LessThanOrEqual | 
                Operator::GreaterThan | Operator::GreaterThanOrEqual => {
                    output.push_str("  cmp rax, rdi\n");
                    match op {
                        Operator::Equal => output.push_str("  sete al\n"),
                        Operator::NotEqual => output.push_str("  setne al\n"),
                        Operator::LessThan => output.push_str("  setl al\n"),
                        Operator::LessThanOrEqual => output.push_str("  setle al\n"),
                        Operator::GreaterThan => output.push_str("  setg al\n"),
                        Operator::GreaterThanOrEqual => output.push_str("  setge al\n"),
                        _ => panic!("Unexpected relational operator"),
                    }
                    output.push_str("  movzb rax, al\n");
                    return output;
                }
                Operator::LogicalOr | Operator::LogicalAnd => {
                    match op {
                        Operator::LogicalOr => {
                            output.push_str("  or rdi, rax\n");
                            output.push_str("  setne al\n");
                            output.push_str("  movzb rax, al\n");
                        }
                        Operator::LogicalAnd => {
                            output.push_str("  cmp rdi, 0\n");
                            output.push_str("  setne dil\n");
                            output.push_str("  cmp rax, 0\n");
                            output.push_str("  setne al\n");
                            output.push_str("  movzb rax, al\n");
                            output.push_str("  and al, dil\n");
                        }
                        _ => panic!("Unexpected logical binary operator"),
                    }
                    
                    return output;
                }
                _ => panic!("Unexpected binary operator"),
            }
        }
        Expression::AssignOp(_op, name, expr) => {
            // TODO: implement compound assignment operators
            let mut output = generate_expression(expr, var_map, stack_index);
            
            if !var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *var_map.get(name).expect("Missing offset");
                output.push_str(&format!("  mov [rbp{}], rax\n", offset));
            }

            return output;
        }
        Expression::Variable(name) => {
            if !var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *var_map.get(name).expect("Missing offset");
                return format!("  mov rax, [rbp{}]\n", offset);
            }
        }
        Expression::TernaryOp(e1, e2, e3) => {
            let mut output = generate_expression(e1, var_map, stack_index);
            output.push_str("  cmp rax, 0\n");

            let e_label = unique("e_");
            output.push_str(&format!("  je {}\n", e_label));
            
            output.push_str(&generate_expression(e2, var_map, stack_index));
            let post_conditional_label = unique("post_conditional_");
            output.push_str(&format!("  jmp {}\n", post_conditional_label));
            
            output.push_str(&format!("{}:\n", e_label));
            output.push_str(&generate_expression(e3, var_map, stack_index));
            
            output.push_str(&format!("{}:\n", post_conditional_label));
            return output;
        }
    }
}

fn generate_function_epilogue(output: &mut String) {
    output.push_str("  mov rsp, rbp\n");
    output.push_str("  pop rbp\n");
    output.push_str("  ret\n");
}

