use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;
use codecrafters_interpreter::frontend::ast_printer::AstPrinter;
use codecrafters_interpreter::frontend::parser::Parser;
use codecrafters_interpreter::frontend::scanner::Scanner;
use codecrafters_interpreter::frontend::stream::{CharStream, Stream};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::FAILURE;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let file_contents = read_file(filename);
            let mut scanner = Scanner::new(CharStream::new(file_contents));
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
        "parse" => {
            let file_contents = read_file(filename);
            let scanner = Scanner::new(CharStream::new(file_contents));
            let mut parser = Parser::new(scanner);

            match parser.expression() {
                Ok(ast) => {
                    let ast_printer = AstPrinter::new();
                    ast.accept(&ast_printer);
                    ExitCode::SUCCESS
                },
                Err(_) => ExitCode::from(65),
            }
        }
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
