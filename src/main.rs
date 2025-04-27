mod bytecode;
mod cli;
mod codegen;
mod constants;
mod error;
mod syntax;
mod tir;
mod typecheck;
mod types;
mod vm;

use clap::Parser;
use cli::{Cli, Cmd, EmitStage};
use codegen::Compiler;
use syntax::{Lexer, Parser as SyntaxParser};
use typecheck::TypeChecker;
use vm::{CallFrame, Prism, TypeDef};

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Cmd::Run { file, emit } => run_program(file.display().to_string(), cli.debug, emit),
        _ => todo!(),
    }
}

fn run_program(
    file: String,
    debug: bool,
    emit: Option<EmitStage>,
) -> Result<(), Box<dyn std::error::Error>> {
    let source =
        std::fs::read_to_string(&file).map_err(|e| format!("Could not read '{}': {e}", file))?;

    // FIXME: Pipeline flow
    let lexer = Lexer::new(&source, &file);
    let mut parser = SyntaxParser::new(&file, &source, lexer);
    let ast_module = parser.parse()?;

    match emit {
        Some(EmitStage::Ast) => {
            println!("{:?}", ast_module);
            return Ok(());
        }
        _ => {}
    }

    let mut type_checker = TypeChecker::new(&source);
    let checked_module = type_checker.check(ast_module.clone())?;

    let code_gen = Compiler::new(&source, checked_module.ast);
    code_gen.borrow_mut().type_defs = type_checker.type_defs.clone();

    let _start = code_gen.borrow_mut().compile()?;

    let mut prism = Prism::new();
    prism.debug_trace = debug;
    prism.globals = code_gen.borrow_mut().globals.clone();
    prism.refraction_methods = code_gen.borrow_mut().refraction_methods.clone();
    prism.facet_layouts = code_gen
        .borrow_mut()
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
