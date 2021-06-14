use std::env;
use std::process::exit;

use closeyc::frontend::parser;

pub static DEBUG: bool = false;

fn main() {
    let mut args = env::args();
    let name = args.next().unwrap();

    if args.len() > 0 {
        match args.next().unwrap().as_str() {
            "build" => (),
            "check" => (),

            "exec" => {
                match args.next() {
                    Some(s) => {
                        let ast = match parser::parse(&s) {
                            Ok(v) => v,

                            Err(_) => {
                                eprintln!("Error parsing!");
                                exit(1);
                            }
                        };

                        println!("{:?}", ast);
                    }

                    None => {
                        eprintln!("Must provide command with exec");
                        exit(1);
                    }
                }

            }

            "help" => {
                println!(
                    "usage:
{}                          - opens up the repl
{} build [options] [files]  - builds the given files
{} check [files]            - checks the given files for correctness
{} exec [command]           - executes closey code
{} help                     - displays this message
{} run [options] [files]    - runs the given files
{} version                  - displays the version number",
                    name, name, name, name, name, name, name
                );
            }

            "run" => (),
            "version" => (),

            _ => {
                eprintln!("Invalid option. See {} help for help.", name);
                exit(1);
            }
        }
    } else {
        todo!("repl");
    }
}

