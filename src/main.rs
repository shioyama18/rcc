extern crate rcc;
extern crate peek_nth;

use std::fs::File;
use std::io::prelude::*;
use std::io::Error;

use rcc::lexer::*;
use rcc::parser::*;
use rcc::generator::*;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.len() != 1 {
        eprintln!("Error: Invalid number of arguments");
        std::process::exit(1);
    }
    
    let tokens = match read_file(&args[0]) {
        Ok(s) => lex(&s),
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    };
    // println!("{:#?}", tokens);
    let ast = parse(&tokens);
    //println!("{:#?}", ast);

    let generated = generate(&ast);
    println!("{}", generated);
}

fn read_file(input: &str) -> Result<String, Error> {
    let mut file = File::open(input)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
