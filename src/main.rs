use mmap::{
    MapOption::{MapExecutable, MapReadable, MapWritable},
    MemoryMap,
};
use std::env;
use std::process::exit;

use closeyc::backends::ir as backend_ir;
use closeyc::backends::x86::codegen;
use closeyc::frontend::correctness;
use closeyc::frontend::ir::{self as frontend_ir, Ir};
use closeyc::frontend::parser;

pub static DEBUG: bool = false;

enum ExecMode {
    Exec,
    Correctness,
    Ir,
    Codegen,
    All,
}

fn main() {
    let mut args = env::args();
    let name = args.next().unwrap();

    if args.len() > 0 {
        match args.next().unwrap().as_str() {
            "build" => (),
            "check" => (),

            "exec" => match args.next() {
                Some(s) => {
                    let mode = if let Some(m) = args.next() {
                        match m.as_str() {
                            "analyse" | "analyze" => ExecMode::Correctness,
                            "ir" => ExecMode::Ir,
                            "codegen" => ExecMode::Codegen,
                            "all" => ExecMode::All,
                            _ => ExecMode::Exec,
                        }
                    } else {
                        ExecMode::Exec
                    };

                    let ast = match parser::parse(&s) {
                        Ok(v) => v,

                        Err(_) => {
                            eprintln!("Error parsing!");
                            exit(1);
                        }
                    };

                    let mut root = Ir::default();
                    match frontend_ir::convert_ast_to_ir("Main", &s, ast, &mut root) {
                        Ok(v) => v,
                        Err(_) => {
                            eprintln!("Error creating ir!");
                            exit(1);
                        }
                    };

                    let _ = correctness::check_correctness(&mut root, true);
                    if let ExecMode::Correctness = mode {
                        println!("{}", root);
                        return;
                    } else if let ExecMode::All = mode {
                        println!("{}", root);
                    }

                    let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                        root.modules.into_iter().next().unwrap().1,
                    );
                    if let ExecMode::Ir = mode {
                        println!("{}", module);
                        return;
                    } else if let ExecMode::All = mode {
                        println!("{}", module);
                    }

                    let mut code = codegen::generate_code(&mut module);
                    let map =
                        MemoryMap::new(code.len(), &[MapExecutable, MapReadable, MapWritable])
                            .unwrap();
                    code.relocate(map.data());
                    if let ExecMode::Codegen = mode {
                        code.print_data();
                        return;
                    } else if let ExecMode::All = mode {
                        code.print_data();
                    }

                    unsafe {
                        std::ptr::copy(code.as_ptr(), map.data(), code.len());
                        let exec = code.get_fn("main", map.data()).unwrap();
                        println!("{:#x}", exec());
                    }
                }

                None => {
                    eprintln!("Must provide command with exec");
                    exit(1);
                }
            },

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
