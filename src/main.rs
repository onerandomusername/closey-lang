use clap::{crate_version, App, Arg, SubCommand};
use faerie::{ArtifactBuilder, Decl, Link};
use std::env;
use std::fs::{self, File};
use std::process::exit;
use target_lexicon::Triple;

#[allow(unused_imports)]
use closeyc::backends::{
    aarch64, ir as backend_ir, riscv64, wasm64, x86_64, GeneratedCode, DEFAULT_ARCH,
};
use closeyc::frontend::correctness;
use closeyc::frontend::ir::{self as frontend_ir, Ir};
use closeyc::frontend::parser;

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
static MAP_JIT: i32 = 0x0800;
#[cfg(not(any(target_os = "macos", target_arch = "aarch64")))]
static MAP_JIT: i32 = 0;

extern "C" {
    fn pthread_jit_write_protect_np(_: bool);
}

#[derive(Debug)]
enum CloseyCode<'a> {
    None,
    Exec(&'a str),
    Files(Vec<&'a str>),
}

fn main() {
    let files = Arg::with_name("files")
        .multiple(true)
        .last(true)
        .required_unless("exec");
    let exec = Arg::with_name("exec")
        .long("exec")
        .short("e")
        .min_values(1)
        .max_values(1);
    let app =
        App::new("closeyc")
            .version(crate_version!())
            .about("Compiler for the Closey language.")
            .subcommand(
                SubCommand::with_name("build")
                    .about("Builds Closey code and exports as an object file.")
                    .arg(
                        Arg::with_name("output")
                            .long("output")
                            .short("o")
                            .help("The output file; by default this is a.out")
                            .min_values(1)
                            .max_values(1),
                    )
                    .arg(files.clone().help("The Closey files to compile."))
                    .arg(exec.clone().help("A Closey command to compile.")),
            )
            .subcommand(
                SubCommand::with_name("run")
                    .about("Runs Closey code by JIT compiling it.")
                    .arg(files.clone().help("The Closey files to run."))
                    .arg(exec.clone().help("A Closey command to run.")),
            )
            .subcommand(
                SubCommand::with_name("analyse")
                    .alias("analyze")
                    .about("Runs the semantic analyser on the given Closey code")
                    .arg(
                        Arg::with_name("hlir")
                            .long("hlir")
                            .short("i")
                            .help("Prints out the higher level IR"),
                    )
                    .arg(files.clone().help("The Closey files to analyse."))
                    .arg(exec.clone().help("The Closey command to analyse.")),
            )
            .subcommand(
                SubCommand::with_name("assembly")
                    .alias("asm")
                    .about("Prints out the assembly for the given Closey code")
                    .arg(
                        files
                            .clone()
                            .help("The Closey files to generate assembly for."),
                    )
                    .arg(
                        exec.clone()
                            .help("The Closey command to generate assembly for."),
                    ),
            )
            .subcommand(
                SubCommand::with_name("llir")
                    .about("Prints out the low level IR for the given Closey code")
                    .arg(files.help("The Closey files to generate LLIR for."))
                    .arg(exec.help("The Closey command to generate LLIR for.")),
            )
            .subcommand(SubCommand::with_name("repl").about(
                "Runs the Closey REPL. If no subcommand is provided, the REPL will still run.",
            ));

    let matches = app.get_matches();

    let code = match matches.subcommand_name() {
        Some("repl") | None => CloseyCode::None,

        Some(s) => {
            let matches = matches.subcommand_matches(s).unwrap();
            match matches.value_of("exec") {
                Some(v) => CloseyCode::Exec(v),
                None => CloseyCode::Files(matches.values_of("files").unwrap().collect()),
            }
        }
    };

    let contents = match code {
        CloseyCode::Exec(s) => Some(s.to_owned()),
        CloseyCode::Files(v) => match fs::read_to_string(v.first().unwrap()) {
            Ok(s) => Some(s),
            Err(e) => {
                eprintln!("error reading file {}: {}", v.first().unwrap(), e);
                exit(1);
            }
        },
        CloseyCode::None => None,
    };

    match matches.subcommand_name() {
        Some("analyse") => {
            let contents = contents.unwrap();
            let root = check(&contents);
            println!("{}", root);
        }

        Some("assembly") => {
            let contents = contents.unwrap();
            let root = check(&contents);

            let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                root.modules.into_iter().next().unwrap().1,
            );

            let mut code = match compile(&mut module) {
                Some(v) => v,
                None => return,
            };

            match DEFAULT_ARCH {
                "aarch64" => todo!(),
                "riscv64" => todo!(),
                "wasm64" => todo!(),
                "x86_64" => x86_64::codegen::relocate(&mut code),
                _ => panic!("unsupported architecture!"),
            }

            match DEFAULT_ARCH {
                "aarch64" => todo!(),
                "riscv64" => todo!(),
                "wasm64" => todo!(),
                "x86_64" => x86_64::disassemble(&code, std::ptr::null()),
                _ => panic!("unsupported architecture!"),
            }
        }

        Some("build") => {
            let contents = contents.unwrap();
            let root = check(&contents);

            let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                root.modules.into_iter().next().unwrap().1,
            );

            let mut code = match compile(&mut module) {
                Some(v) => v,
                None => return,
            };

            match DEFAULT_ARCH {
                "aarch64" => todo!(),
                "riscv64" => todo!(),
                "wasm64" => todo!(),
                "x86_64" => x86_64::codegen::generate_start_func(&mut code),
                _ => panic!("unsupported architecture!"),
            }

            let f = matches
                .subcommand_matches("build")
                .unwrap()
                .value_of("output")
                .unwrap_or("a.o")
                .to_owned();

            let mut artefact = ArtifactBuilder::new(Triple::host())
                .name(f.clone())
                .finish();

            let mut funcs: Vec<_> = code.get_funcs().iter().collect();
            funcs.sort_by(|a, b| a.1.start.cmp(&b.1.start));
            match artefact.declarations({
                funcs.iter().map(|v| {
                    (
                        v.0,
                        if v.0 == "_start" || v.0 == "main" {
                            Decl::function().global().into()
                        } else if v.1.start == 0 && v.1.end == 0 {
                            Decl::function_import().into()
                        } else {
                            Decl::function().into()
                        },
                    )
                })
            }) {
                Ok(_) => (),
                Err(e) => {
                    eprintln!("Error declaring functions: {}", e);
                    return;
                }
            }

            for (func, range) in funcs {
                if range.start == 0 && range.end == 0 {
                    continue;
                }

                match artefact.define(func, code.data()[range.start..range.end].to_owned()) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Error defining function: {}", e);
                        return;
                    }
                }
            }

            for (addr, to) in code.get_relocation_table() {
                for (from, range) in code.get_funcs() {
                    if range.start <= *addr && *addr < range.end {
                        match artefact.link(Link {
                            from,
                            to,
                            at: (addr - range.start) as u64,
                        }) {
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

        Some("llir") => {
            let contents = contents.unwrap();
            let root = check(&contents);

            let module = backend_ir::convert_frontend_ir_to_backend_ir(
                root.modules.into_iter().next().unwrap().1,
            );
            println!("{}", module);
        }

        Some("run") => {
            let contents = contents.unwrap();
            let root = check(&contents);

            let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                root.modules.into_iter().next().unwrap().1,
            );

            let mut code = match compile(&mut module) {
                Some(v) => v,
                None => return,
            };

            let map = unsafe {
                libc::mmap(
                    std::ptr::null_mut(),
                    code.len(),
                    libc::PROT_WRITE | libc::PROT_READ,
                    libc::MAP_ANONYMOUS | libc::MAP_PRIVATE | MAP_JIT,
                    0,
                    0,
                )
            } as *mut u8;
            match DEFAULT_ARCH {
                "aarch64" => todo!(),
                "riscv64" => todo!(),
                "wasm64" => todo!(),
                "x86_64" => x86_64::codegen::relocate(&mut code),
                _ => panic!("unsupported architecture!"),
            }

            unsafe {
                pthread_jit_write_protect_np(false);
                std::ptr::copy(code.data().as_ptr(), map, code.len());
                pthread_jit_write_protect_np(true);
                libc::mprotect(
                    map as *mut libc::c_void,
                    code.len(),
                    libc::PROT_READ | libc::PROT_EXEC,
                );
                let exec = code.get_fn("main", map).unwrap();
                let v = exec();
                println!("{:#x}", v);
            }

            unsafe {
                libc::munmap(map as *mut libc::c_void, code.len());
            }
        }

        Some("repl") | None => todo!(),
        _ => unreachable!("Invalid subcommand"),
    }
}

fn check(s: &str) -> Ir {
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
    root
}

fn compile(module: &mut backend_ir::IrModule) -> Option<GeneratedCode> {
    match DEFAULT_ARCH {
        "aarch64" => Some(aarch64::codegen::generate_code(module)),
        "riscv64" => todo!(),
        "wasm64" => todo!(),
        "x86_64" => Some(x86_64::codegen::generate_code(module)),
        _ => panic!("unsupported architecture"),
    }
}
