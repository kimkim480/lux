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
use prism::Prism;
use std::{env, fs};
use value::CallFrame;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args().skip(1);
    let filename = args.next().ok_or("Usage: prism <filename>")?;
    let debug = args.any(|arg| arg == "--debug");

    let source = fs::read_to_string(&filename)
        .map_err(|e| format!("Could not read file '{}': {}", filename, e))?;

    let compiler = Compiler::new(&filename, source)?;
    let _start = compiler.borrow_mut().compile()?;

    let mut prism = Prism::new();
    prism.debug_trace = debug;
    prism.globals = compiler.borrow().globals.clone();

    prism.frames.push(CallFrame {
        function: _start.clone(),
        ip: 0,
        offset: 0,
        upvalues: vec![],
    });

    prism.run()?;

    Ok(())
}
