use faerie::{ArtifactBuilder, Decl, Link};
use target_lexicon::Triple;
use mmap::{
    MapOption::{MapExecutable, MapReadable, MapWritable},
    MemoryMap,
};
use std::env;
use std::fs::{self, File};
use std::process::exit;

#[allow(unused_imports)]
use closeyc::backends::{DEFAULT_ARCH, GeneratedCode, ir as backend_ir, aarch64, riscv64, x86_64, wasm64};
use closeyc::frontend::correctness;
use closeyc::frontend::ir::{self as frontend_ir, Ir};
use closeyc::frontend::parser;

#[derive(Clone, Copy)]
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
            "build" => match args.next() {
                Some(mut f) => {
                    let contents = match fs::read_to_string(&f) {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!("Error reading file {}: {}", f, e);
                            exit(1);
                        }
                    };

                    let root = check(&contents, ExecMode::Exec);
                    let mut code = match compile(root, ExecMode::Exec) {
                        Some(v) => v,
                        None => return
                    };

                    match DEFAULT_ARCH {
                        "aarch64" => todo!(),
                        "riscv64" => todo!(),
                        "wasm64" => todo!(),
                        "x86_64" => x86_64::codegen::generate_start_func(&mut code),
                        _ => panic!("unsupported architecture!")
                    }

                    code.relocate(std::ptr::null());
                    f.push_str(".o");

                    let mut artefact = ArtifactBuilder::new(Triple::host())
                        .name(f.clone())
                        .finish();

                    match artefact.declarations({
                        let mut v: Vec<_> = code.get_funcs().iter().collect();
                        v.sort_by(|a, b| a.1.start.cmp(&b.1.start));
                        v.into_iter().map(|v| (v.0,
                            if v.0 == "main" || v.0 == "_start" {
                                Decl::function().global().into()
                            } else if v.1.start == 0 && v.1.end == 0 {
                                Decl::function_import().into()
                            } else {
                                Decl::function().into()
                            }))
                    }) {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Error declaring functions: {}", e);
                            return;
                        }
                    }

                    for (func, range) in code.get_funcs() {
                        if range.start == 0 && range.end == 0 {
                            continue;
                        }

                        match artefact.define(func, code.data()[range.start..range.end].to_owned()) {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("Error defining function: {}", e);
                                return
                            }
                        }
                    }

                    for (addr, (to, _rel)) in code.get_relocation_table() {
                        for (from, range) in code.get_funcs() {
                            if range.start <= *addr && *addr < range.end {
                                match artefact.link(Link { from, to, at: (addr - range.start) as u64 }) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        eprintln!("Error linking: {}", e);
                                        return;
                                    }
                                }
                                break;
                            }
                        }
                    }

                    match artefact.write(match File::create(&f) {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!("Error getting file {}: {}", f, e);
                            exit(1);
                        }
                    }) {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Error writing artefact to file: {}", e);
                        }
                    }
                }

                None => {
                    eprintln!("Must provide file name with build");
                    exit(1);
                }
            },

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

                    let root = check(&s, mode);
                    let mut code = match compile(root, mode) {
                        Some(v) => v,
                        None => return
                    };

                    let map =
                        MemoryMap::new(code.len(), &[MapExecutable, MapReadable, MapWritable])
                            .unwrap();
                    code.relocate(map.data());
                    if let ExecMode::Codegen = mode {
                        match DEFAULT_ARCH {
                            "aarch64" => todo!(),
                            "riscv64" => todo!(),
                            "wasm64" => todo!(),
                            "x86_64" => x86_64::disassemble(&code, map.data()),
                            _ => panic!("unsupported architecture!")
                        }
                        return;
                    } else if let ExecMode::All = mode {
                        match DEFAULT_ARCH {
                            "aarch64" => todo!(),
                            "riscv64" => todo!(),
                            "wasm64" => todo!(),
                            "x86_64" => x86_64::disassemble(&code, map.data()),
                            _ => panic!("unsupported architecture!")
                        }
                    }

                    unsafe {
                        std::ptr::copy(code.data().as_ptr(), map.data(), code.len());
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

fn check(s: &str, mode: ExecMode) -> Ir {
    let ast = match parser::parse(s) {
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
    } else if let ExecMode::All = mode {
        println!("{}", root);
    }

    root
}

fn compile(root: Ir, mode: ExecMode) -> Option<GeneratedCode> {
    let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
        root.modules.into_iter().next().unwrap().1,
    );

    if let ExecMode::Ir = mode {
        println!("{}", module);
        return None;
    } else if let ExecMode::All = mode {
        println!("{}", module);
    }

    match DEFAULT_ARCH {
        "aarch64" => Some(aarch64::codegen::generate_code(&mut module)),
        "riscv64" => todo!(),
        "wasm64" => todo!(),
        "x86_64" => Some(x86_64::codegen::generate_code(&mut module)),
        _ => panic!("unsupported architecture")
    }
}
