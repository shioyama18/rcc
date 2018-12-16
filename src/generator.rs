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
            let suffix = unique_suffix();
            let post_if_label = add_suffix("post_if", &suffix);
            let post_else_label = add_suffix("post_else", &suffix);

            generate_expression(expr, &context);
            println!("  cmp rax, 0");
            println!("  je {}", post_if_label);
            generate_statement(if_body, &context);

            if let Some(else_statement) = else_body {
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
        Statement::While(condition, body) => {
            loop_helper(condition, &None, body, &context);
        }
        Statement::DoWhile(body, condition) => {
            let suffix = unique_suffix();
            let loop_label = add_suffix("loop", &suffix);
            let break_label = add_suffix("post_loop", &suffix);
            let continue_label = add_suffix("continue_do_while", &suffix);

            println!("{}:", loop_label);

            let body_context = Context {
                break_label: Some(break_label.clone()),
                continue_label: Some(continue_label.clone()),
                ..context.clone()
            };
            generate_statement(body, &body_context);
            generate_expression(condition, &context);
            println!("  cmp rax, 0");
            println!("  jne {}", loop_label);
            println!("{}:", break_label);
        }
        Statement::For(init, condition, post_expression, body) => {
            if let Some(expr) = init {
                generate_expression(expr, &context);
            }

            loop_helper(condition, post_expression, body, &context);
        }
        Statement::ForDeclaration(decl, condition, post_expression, body) => {
            let mut context: Context = context.clone();
            generate_declaration(decl, &mut context);
            
            loop_helper(condition, post_expression, body, &context);
            println!("  pop rax");
        }
        Statement::Break => {
            match context.break_label {
                Some(label) => println!("  jmp {}", label),
                None => panic!("Break statement not in loop"),
            }
        }
        Statement::Continue => {
            match context.continue_label {
                Some(label) => println!("  jmp {}", label),
                None => panic!("Continue statement not in loop"),
            }
        }
    }
}

fn loop_helper(condition: &Expression, post_expression: &Option<Expression>, body: &Statement, context: &Context) {
    let suffix = unique_suffix();
    let loop_label = add_suffix("loop", &suffix);
    let post_loop_label = add_suffix("post_loop", &suffix);
    let continue_label = add_suffix("loop_continue", &suffix);

    println!("{}:", loop_label);
    generate_expression(condition, context);
    println!("  cmp rax, 0");
    println!("  je {}", post_loop_label);

    let body_context = Context {
        break_label: Some(post_loop_label.clone()),
        continue_label: Some(continue_label.clone()),
        ..context.clone()
    };

    generate_statement(body, &body_context);

    println!("{}:", continue_label);

    if let Some(expr) = post_expression {
        generate_expression(expr, &context);
    }

    println!("  jmp {}", loop_label);
    println!("{}:", post_loop_label);
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
                Operator::Plus | Operator::Minus | Operator::Multiplication | Operator::Division | Operator::Modulo => {
                    match op {
                        Operator::Plus => println!("  add rax, rdi"),
                        Operator::Minus => println!("  sub rax, rdi"),
                        Operator::Multiplication => println!("  mul rdi"),
                        Operator::Division => {
                            println!("  mov rdx, 0");
                            println!("  div rdi");
                        }
                        Operator::Modulo => {
                            println!("  mov rdx, 0");
                            println!("  div rdi");
                            println!("  mov rax, rdx");
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

            let suffix = unique_suffix();
            let e_label = add_suffix("e", &suffix);
            let post_conditional_label = add_suffix("post_conditional", &suffix);

            println!("  je {}", e_label);
            generate_expression(e2, context);
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

