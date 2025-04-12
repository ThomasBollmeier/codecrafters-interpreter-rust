use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;
use codecrafters_interpreter::common::ErrorContext;
use codecrafters_interpreter::frontend::ast_printer::AstPrinter;
use codecrafters_interpreter::frontend::parser::Parser;
use codecrafters_interpreter::frontend::scanner::Scanner;
use codecrafters_interpreter::frontend::stream::{CharStream, Stream};
use codecrafters_interpreter::interpreter::Interpreter;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::FAILURE;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => tokenize(read_file(filename)),
        "parse" => parse(read_file(filename)),
        "evaluate" => evaluate(read_file(filename)),
        _ => {
            eprintln!("Unknown command: {}", command);
            ExitCode::FAILURE
        }
    }
}

fn read_file(filename: &str) -> String {
    fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    })
}

fn tokenize(code: String) -> ExitCode {
    let mut scanner = Scanner::new(CharStream::new(code));
    let mut exit_code = ExitCode::SUCCESS;

    loop {
        match scanner.next() {
            Ok(token_opt) => {
                match token_opt {
                    Some(token) => println!("{token}"),
                    None => break,
                }
            }
            Err(err) => {
                writeln!(io::stderr(), "[line {}] Error: {}",
                         err.get_line(), err.get_message())
                    .expect("cannot write error");
                exit_code = ExitCode::from(65);
            }
        }
    }
    println!("EOF  null");
    exit_code
}

fn parse(code: String) -> ExitCode {
    let scanner = Scanner::new(CharStream::new(code));
    let mut parser = Parser::new(scanner);

    match parser.expression() {
        Ok(ast) => {
            let mut ast_printer = AstPrinter::new();
            ast.accept(&mut ast_printer);
            ExitCode::SUCCESS
        },
        Err(_) => ExitCode::from(65),
    }
}

fn evaluate(code: String) -> ExitCode {
    let interpreter = Interpreter::new();
    let result = interpreter.eval(code);
    
    match result {
        Ok(value) => {
            println!("{}", value);
            ExitCode::SUCCESS
        }
        Err(err) => {
            eprintln!("{}", err.get_message());
            match err.get_context() {
                ErrorContext::Interpreter => ExitCode::from(70),
                _ => ExitCode::from(65),
            }
        }
    }
}
