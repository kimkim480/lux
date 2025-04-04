mod ast;
mod compiler;
mod constants;
mod error;
mod lexer;
mod parser;
mod prism;
mod token;
mod value;

use compiler::Compiler;
use prism::{Prism, PrismError};
use std::fs;
use value::CallFrame;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(source) => match Compiler::new(source) {
                Ok(mut compiler) => {
                    let _start = compiler.compile();
                    let mut prism = Prism::new();
                    prism.frames.push(CallFrame {
                        function: _start,
                        ip: 0,
                        offset: 0,
                    });

                    prism.globals = compiler.globals;

                    match prism.run() {
                        Err(PrismError::Compile(msg)) => eprintln!("💥 Compile error: {msg}"),
                        Err(PrismError::Runtime(msg)) => eprintln!("💥 Runtime error: {msg}"),
                        _ => {}
                    }
                }
                Err(err) => eprintln!("{}", err),
            },
            Err(err) => {
                eprintln!("Could not read file '{filename}': {err}");
            }
        }
    } else {
        eprintln!("Usage: prism <filename>");
    }
}
