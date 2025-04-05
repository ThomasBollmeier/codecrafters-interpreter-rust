use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;
use codecrafters_interpreter::frontend::scanner::Scanner;
use codecrafters_interpreter::frontend::stream::{CharStream, Stream};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return ExitCode::FAILURE;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });
            
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
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            ExitCode::FAILURE
        }
    }
}
