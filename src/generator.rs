use util::*;
use token::*;
use ast::*;
use context::*;

pub fn generate(ast: &Program) {
    println!(".intel_syntax noprefix");

    match ast {
        Program::Program(FunctionDeclaration::Function(name, block)) => {
            generate_function(name, block);
        }
    }
}

fn generate_function(name: &String, block: &Block) {
    let context = Context::new();

    println!(".global {}", name);
    println!("{}:", name);

    println!("  push rbp");
    println!("  mov rbp, rsp");

    generate_block(block, &context);

    println!("  mov rax, 0");
    generate_function_epilogue();
}

fn generate_block(block: &Block, context: &Context) {
    let mut context = context.clone();

    for block_item in block {
        match block_item {
            BlockItem::Declaration(declaration) => {
                generate_declaration(declaration, &mut context);
            }
            BlockItem::Statement(statement) => {
                generate_statement(statement, &context);
            }
        }
    }

    let bytes_to_deallocate = 8 * context.current_scope.len();
    println!("  add rsp, {}", bytes_to_deallocate);
}

fn generate_declaration(declaration: &Declaration, context: &mut Context) {
    match declaration {
        Declaration::Declare(name, value) => {
            if context.current_scope.contains(name) {
                panic!("Variable {} declared twice in same scope", name);
            }

            if let Some(expr) = value {
                generate_expression(expr, &context);
                println!("  push rax");
            } else {
                println!("  push 0");
            }

            context.var_map.insert(name.clone(), context.stack_index);
            context.current_scope.insert(name.clone());
            context.stack_index -= 8;
        }
    }
}

fn generate_statement(statement: &Statement, context: &Context) {
    let context = context.reset_scope();
    match statement {
        Statement::Return(expr) => {
            generate_expression(expr, &context);
            generate_function_epilogue();
        }
        Statement::Expression(expr) => {
            if let Some(e) = expr {
                generate_expression(e, &context);
            }
        }
        Statement::Conditional(expr, if_body, else_body) => {
            let post_if_label = unique("post_if_");

            generate_expression(expr, &context);
            println!("  cmp rax, 0");
            println!("  je {}", post_if_label);
            generate_statement(if_body, &context);

            if let Some(else_statement) = else_body {
                let post_else_label = unique("post_else_");
                println!("  jmp {}", post_else_label);
                
                println!("{}:", post_if_label);
                generate_statement(else_statement, &context);
                
                println!("{}:", post_else_label);
            } else {
                println!("{}:", post_if_label);
            }
        }
        Statement::Compound(block) => {
            generate_block(block, &context);
        }
        Statement::While(expr, body) => {
            let while_label = unique("while_");
            let post_while_label = unique("post_while_");
            
            println!("{}:", while_label);
            generate_expression(expr, &context);
            println!("  cmp rax, 0");
            println!("  je {}", post_while_label);
            generate_statement(body, &context);
            println!("  jmp {}", while_label);            
            println!("{}:", post_while_label);
        }
        Statement::Do(body, expr) => {
            let do_while_label = unique("do_while_");
            println!("{}:", do_while_label);
            generate_statement(body, &context);
            generate_expression(expr, &context);
            println!("  cmp rax, 0");
            println!("  jne {}", do_while_label);
        }
        Statement::For(init, condition, post_expression, body) => {
            let for_label = unique("for_");
            let post_for_label = unique("post_for_");

            if let Some(expr) = init {
                generate_expression(expr, &context);
            }

            println!("{}:", for_label);
            generate_expression(condition, &context);
            println!("  cmp rax, 0");
            println!("  je {}", post_for_label);
            generate_statement(body, &context);

            if let Some(expr) = post_expression {
                generate_expression(expr, &context);
            }

            println!("  jmp {}", for_label);
            println!("{}:", post_for_label);
        }
        Statement::ForDeclaration(decl, condition, post_expression, body) => {
            let mut context: Context = context.clone();
            let for_label = unique("for_");
            let post_for_label = unique("post_for_");
            
            generate_declaration(decl, &mut context);
            
            println!("{}:", for_label);
            generate_expression(condition, &context);
            println!("  cmp rax, 0");
            println!("  je {}", post_for_label);
            generate_statement(body, &context);

            if let Some(expr) = post_expression {
                generate_expression(expr, &context);
            }

            println!("  jmp {}", for_label);
            println!("  pop rax");
            println!("{}:", post_for_label);
        }
        _ => unimplemented!(),
    }
}

fn generate_expression(expression: &Expression, context: &Context) {
    match expression {
        Expression::Constant(n) => println!("  mov rax, {}", n),
        Expression::UnaryOp(op, expr) => {
            generate_expression(expr, context);

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
        }
        Expression::BinaryOp(op, lhs, rhs) => {
            generate_expression(rhs, context);
            println!("  push rax");
            generate_expression(lhs, context);
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
                }
                _ => panic!("Unexpected binary operator"),
            }
        }
        Expression::AssignOp(_op, name, expr) => {
            // TODO: implement compound assignment operators
            generate_expression(expr, context);
            
            if !context.var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *context.var_map.get(name).expect("Missing offset");
                println!("  mov [rbp{}], rax", offset);
            }
        }
        Expression::Variable(name) => {
            if !context.var_map.contains_key(name) {
                panic!("Variable undeclared");
            } else {
                let offset: isize = *context.var_map.get(name).expect("Missing offset");
                println!("  mov rax, [rbp{}]", offset);
            }
        }
        Expression::TernaryOp(e1, e2, e3) => {
            generate_expression(e1, context); 
            println!("  cmp rax, 0");

            let e_label = unique("e_");
            println!("  je {}", e_label);
            
            generate_expression(e2, context);
            let post_conditional_label = unique("post_conditional_");
            println!("  jmp {}", post_conditional_label);
            
            println!("{}:", e_label);
            generate_expression(e3, context);
            
            println!("{}:", post_conditional_label);
        }
    }
}

fn generate_function_epilogue() {
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

