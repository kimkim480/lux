mod ast;
mod compiler;
mod constants;
mod error;
mod lexer;
mod parser;
mod prism;
mod token;
mod typechecker;
mod value;

use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use prism::Prism;
use std::{env, fs};
use typechecker::TypeChecker;
use value::{CallFrame, TypeDef};

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
    // TODO: Use clap for argument parsing
    // collect arguments (excluding program name)
    let args: Vec<String> = env::args().skip(1).collect();
    // flags
    let debug = args.iter().any(|arg| arg == "--debug" || arg == "-d");
    let version = args.iter().any(|arg| arg == "--version" || arg == "-v");
    let help = args.iter().any(|arg| arg == "--help" || arg == "-h");

    // version request: print version and exit
    if version {
        println!("Lux v0.0.1-alpha.1");
        return Ok(());
    }

    // help or no args: print usage and exit
    if help || args.is_empty() {
        println!("Usage: lux <filename>");
        println!("Options:");
        println!("  --debug, -d    Enable debug mode (lux <filename> -d)");
        println!("  --version, -v  Show version information");
        println!("  --help, -h     Show this help message");
        return Ok(());
    }

    // first non-flag argument is filename; if missing, show help
    let filename = if let Some(f) = args.iter().find(|arg| !arg.starts_with('-')) {
        f
    } else {
        println!("Usage: lux <filename>");
        println!("Options:");
        println!("  --debug, -d    Enable debug mode (lux <filename> -d)");
        println!("  --version, -v  Show version information");
        println!("  --help, -h     Show this help message");
        return Ok(());
    };
    let source = fs::read_to_string(&filename)
        .map_err(|e| format!("Could not read file '{}': {}", filename, e))?;

    // TODO: adjust the pipeline
    let lexer = Lexer::new(&source, &filename);
    let mut parser = Parser::new(&filename, lexer);
    let ast = parser.parse()?;

    let mut type_checker = TypeChecker::new(&source);
    type_checker.check(&ast)?;

    let compiler = Compiler::new(&source, ast);
    compiler.borrow_mut().type_defs = type_checker.type_defs;

    let _start = compiler.borrow_mut().compile()?;

    let mut prism = Prism::new();
    prism.debug_trace = debug;
    prism.globals = compiler.borrow().globals.clone();
    prism.methods = compiler.borrow().methods.clone();
    prism.facet_layouts = compiler
        .borrow()
        .type_defs
        .iter()
        .filter_map(|(name, def)| {
            if let TypeDef::Facet { fields } = def {
                Some((
                    name.clone(),
                    fields.iter().map(|(f, _)| f.clone()).collect(),
                ))
            } else {
                None
            }
        })
        .collect();

    prism.frames.push(CallFrame {
        function: _start.clone(),
        ip: 0,
        offset: 0,
        upvalues: vec![],
    });

    prism.run()?;

    Ok(())
}

fn get_source_line(source: &String, line: usize) -> String {
    source.lines().nth(line - 1).unwrap_or("").to_string()
}
