use mmap::{
    MapOption::{MapExecutable, MapReadable, MapWritable},
    MemoryMap,
};
use std::env;
use std::process::exit;

#[allow(unused_imports)]
use closeyc::backends::{Code, ir as backend_ir, aarch64, riscv64, x86_64, wasm64};
use closeyc::frontend::correctness;
use closeyc::frontend::ir::{self as frontend_ir, Ir};
use closeyc::frontend::parser;

enum ExecMode {
    Exec,
    Correctness,
    Ir,
    Codegen,
    All,
}

#[cfg(target_arch = "aarch64")]
const DEFAULT_ARCH: &str = "aarch64";
#[cfg(target_arch = "riscv64")]
const DEFAULT_ARCH: &str = "riscv64";
#[cfg(target_arch = "wasm64")]
const DEFAULT_ARCH: &str = "wasm64";
#[cfg(target_arch = "x86_64")]
const DEFAULT_ARCH: &str = "x86_64";

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

                    let mut code: Box<dyn Code> = match DEFAULT_ARCH {
                        "aarch64" => Box::new(aarch64::codegen::generate_code(&mut module)),
                        "riscv64" => todo!(),
                        "wasm64" => todo!(),
                        "x86_64" => Box::new(x86_64::codegen::generate_code(&mut module)),
                        _ => panic!("unsupported architecture")
                    };

                    let map =
                        MemoryMap::new(code.len(), &[MapExecutable, MapReadable, MapWritable])
                            .unwrap();
                    code.relocate(map.data());
                    if let ExecMode::Codegen = mode {
                        code.disassemble(map.data());
                        return;
                    } else if let ExecMode::All = mode {
                        code.disassemble(map.data());
                    }

                    unsafe {
                        std::ptr::copy(code.as_ptr(), map.data(), code.len());
                        let exec = code.get_fn("main", map.data()).unwrap();
                        let v = exec();
                        println!("{:#x}", v);
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
