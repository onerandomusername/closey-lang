use std::env;
use std::process::exit;

use closeyc::backends::ir as backend_ir;
use closeyc::frontend::ir::{self as frontend_ir, Ir};
use closeyc::frontend::parser;
use closeyc::frontend::correctness;

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
                        let mut root = Ir::default();
                        match frontend_ir::convert_ast_to_ir("Main", &s, ast, &mut root) {
                            Ok(v) => v,
                            Err(_) => {
                                eprintln!("Error creating ir!");
                                exit(1);
                            }
                        };

                        println!("{}", root);
                        let _ = correctness::check_correctness(&mut root, true);
                        println!("{}", root);

                        let module = backend_ir::convert_frontend_ir_to_backend_ir(root.modules.into_iter().next().unwrap().1);
                        println!("{}", module);
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

