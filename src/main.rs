mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod prism;
mod token;
mod value;

use compiler::Compiler;
use prism::Prism;
use std::io::{self, Write};

fn main() {
    repl();
}

fn repl() {
    let debug_mode = true;
    println!("Lux REPL — Post Tenebras Lux");

    loop {
        print!("◢ ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            break;
        }

        if line.trim().is_empty() {
            continue;
        }

        // Try to compile source
        let compiler = match Compiler::new(line.clone()) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Syntax error at {}:{} → {}", e.line, e.column, e.message);
                continue; // ← don't crash! Let user try again
            }
        };

        let chunk = compiler.compile();
        let mut prism = Prism::new(chunk);
        match prism.run() {
            Some(val) => {
                if debug_mode {
                    println!("{:?}", val); // raw output
                } else {
                    println!("=> {}", val); // pretty
                }
            }
            None => eprintln!("Runtime error"),
        }
    }
}
