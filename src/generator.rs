use std::collections::HashMap;

use util::*;
use token::*;
use ast::*;

type VariableMap = HashMap<String, isize>;

pub fn generate(ast: &Program) {
    println!(".intel_syntax noprefix");

    match ast {
        Program::Program(FunctionDeclaration::Function(name, block)) => {
            generate_function(name, block);
        }
    }
}

fn generate_function(name: &String, block: &Block) {
    let mut var_map = VariableMap::new();
    let mut stack_index: isize = -8;

    println!(".global {}", name);
    println!("{}:", name);

    println!("  push rbp");
    println!("  mov rbp, rsp");

    generate_block(block, &mut var_map, &mut stack_index);

    println!("  mov rax, 0");
    generate_function_epilogue();
}

fn generate_block(block: &Block, var_map: &mut VariableMap, stack_index: &mut isize) {
    for block_item in block {
        match block_item {
            BlockItem::Declaration(declaration) => generate_declaration(declaration, var_map, stack_index),
            BlockItem::Statement(statement) => generate_statement(statement, var_map, stack_index),
        }
    }
}

fn generate_declaration(declaration: &Declaration, var_map: &mut VariableMap, stack_index: &mut isize) {
    match declaration {
        Declaration::Declare(name, value) => {
            if var_map.contains_key(name) {
                panic!("Variable {} declared twice in same scope", name);
            }

            if let Some(expr) = value {
                generate_expression(expr, var_map, stack_index);
                println!("  push rax");
            } else {
                println!("  push 0");
            }

            var_map.insert(name.clone(), *stack_index);
            *stack_index -= 8;
        }
    }
}

fn generate_statement(statement: &Statement, var_map: &mut VariableMap, stack_index: &mut isize) {
    match statement {
        Statement::Return(expr) => {
            generate_expression(expr, var_map, stack_index);
            generate_function_epilogue();
        }
        Statement::Expression(expr) => {
            generate_expression(expr, var_map, stack_index);
        }
        Statement::Conditional(expr, if_body, else_body) => {
            let post_if_label = unique("post_if_");

            generate_expression(expr, var_map, stack_index);
            println!("  cmp rax, 0");
            println!("  je {}", post_if_label);
            generate_statement(if_body, var_map, stack_index);

            if let Some(else_statement) = else_body {
                let post_else_label = unique("post_else_");
                println!("  jmp {}", post_else_label);
                
                println!("{}:", post_if_label);
                generate_statement(else_statement, var_map, stack_index);
                
                println!("{}:", post_else_label);
            } else {
                println!("{}:", post_if_label);
            }
        }
        Statement::Compound(block) => {
            generate_block(block, var_map, stack_index);
        }
    }
}

fn generate_expression(expression: &Expression, var_map: &VariableMap, stack_index: &isize) {
    match expression {
        Expression::Constant(n) => println!("  mov rax, {}", n),
        Expression::UnaryOp(op, expr) => {
            let mut output = generate_expression(expr, var_map, stack_index);

            match op {
                Operator::Minus => {
                    println!("  neg rax");
                }
                Operator::BitwiseComplement => {
                    println!("  not rax");
                }
                Operator::LogicalNegation => {
                    println!("  mov rdi, 0");
                    println!("  cmp rdi, rax");
                    println!("  sete al");
                    println!("  movzb rax, al");
                }
                _ => panic!("Unexpected unary operator"),
            }

            return output;
        }
        Expression::BinaryOp(op, lhs, rhs) => {
            let mut output = generate_expression(rhs, var_map, stack_index);
            println!("  push rax");
            generate_expression(lhs, var_map, stack_index);
            println!("  pop rdi");

            match op {
                Operator::Plus | Operator::Minus | Operator::Multiplication | Operator::Division => {
                    match op {
                        Operator::Plus => println!("  add rax, rdi"),
                        Operator::Minus => println!("  sub rax, rdi"),
                        Operator::Multiplication => println!("  mul rdi"),
                        Operator::Division => {
                            println!("  mov rdx, 0");
                            println!("  div rdi");
                        }
                        _ => panic!("Unexpected binary operator"),
                    }
                    return output;
                }
                Operator::Equal | Operator::NotEqual | 
                Operator::LessThan | Operator::LessThanOrEqual | 
                Operator::GreaterThan | Operator::GreaterThanOrEqual => {
                    println!("  cmp rax, rdi");
                    match op {
                        Operator::Equal => println!("  sete al"),
                        Operator::NotEqual => println!("  setne al"),
                        Operator::LessThan => println!("  setl al"),
                        Operator::LessThanOrEqual => println!("  setle al"),
                        Operator::GreaterThan => println!("  setg al"),
                        Operator::GreaterThanOrEqual => println!("  setge al"),
                        _ => panic!("Unexpected relational operator"),
                    }
                    println!("  movzb rax, al");
                    return output;
                }
                Operator::LogicalOr | Operator::LogicalAnd => {
                    match op {
                        Operator::LogicalOr => {
                            println!("  or rdi, rax");
                            println!("  setne al");
                            println!("  movzb rax, al");
                        }
                        Operator::LogicalAnd => {
                            println!("  cmp rdi, 0");
                            println!("  setne dil");
                            println!("  cmp rax, 0");
                            println!("  setne al");
                            println!("  movzb rax, al");
                            println!("  and al, dil");
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
            generate_expression(expr, var_map, stack_index);
            
            if !var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *var_map.get(name).expect("Missing offset");
                println!("  mov [rbp{}], rax", offset);
            }
        }
        Expression::Variable(name) => {
            if !var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *var_map.get(name).expect("Missing offset");
                println!("  mov rax, [rbp{}]", offset);
            }
        }
        Expression::TernaryOp(e1, e2, e3) => {
            generate_expression(e1, var_map, stack_index);
            println!("  cmp rax, 0");

            let e_label = unique("e_");
            println!("  je {}", e_label);
            
            generate_expression(e2, var_map, stack_index);
            let post_conditional_label = unique("post_conditional_");
            println!("  jmp {}", post_conditional_label);
            
            println!("{}:", e_label);
            generate_expression(e3, var_map, stack_index);
            
            println!("{}:", post_conditional_label);
        }
    }
}

fn generate_function_epilogue() {
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

