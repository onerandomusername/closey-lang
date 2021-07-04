use clap::{crate_version, App, Arg, SubCommand};
use faerie::{ArtifactBuilder, Decl, Link};
use rustyline::{error::ReadlineError, Editor};
use std::env;
use std::fs::{self, File};
use std::process::exit;
use target_lexicon::Triple;

#[allow(unused_imports)]
use closeyc::backends::{
    aarch64, ir as backend_ir, riscv64, wasm64, x86_64, GeneratedCode, DEFAULT_ARCH,
};
use closeyc::frontend::correctness;
use closeyc::frontend::ir as frontend_ir;
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

struct Jit {
    code: GeneratedCode,
    mem: *const u8,
}

impl Jit {
    fn new(mut code: GeneratedCode) -> Jit {
        let mem = unsafe {
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
            std::ptr::copy(code.data().as_ptr(), mem, code.len());
            libc::mprotect(
                mem as *mut libc::c_void,
                code.len(),
                libc::PROT_READ | libc::PROT_EXEC,
            );
            pthread_jit_write_protect_np(true);
        }

        Jit { code, mem }
    }

    unsafe fn call(&self, func: &str) -> Option<*const u8> {
        self.code.get_fn(func, self.mem).map(|v| v())
    }
}

impl Drop for Jit {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.mem as *mut libc::c_void, self.code.len());
        }

        self.mem = std::ptr::null_mut();
    }
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
            let mut root = frontend_ir::Ir::new();
            check(&contents, "Main", &mut root);
        }

        Some("assembly") => {
            let contents = contents.unwrap();
            let mut root = frontend_ir::Ir::new();
            check(&contents, "Main", &mut root);

            let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                &root.modules.iter().next().unwrap().1,
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
            let mut root = frontend_ir::Ir::new();
            check(&contents, "Main", &mut root);

            let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                &root.modules.iter().next().unwrap().1,
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
            let mut root = frontend_ir::Ir::new();
            check(&contents, "Main", &mut root);

            let module = backend_ir::convert_frontend_ir_to_backend_ir(
                &root.modules.iter().next().unwrap().1,
            );
            println!("{}", module);
        }

        Some("run") => {
            let contents = contents.unwrap();
            let mut root = frontend_ir::Ir::new();
            check(&contents, "Main", &mut root);

            let mut module = backend_ir::convert_frontend_ir_to_backend_ir(
                &root.modules.iter().next().unwrap().1,
            );

            let code = match compile(&mut module) {
                Some(v) => v,
                None => return,
            };

            let jit = Jit::new(code);
            println!("{:#x}", unsafe { jit.call("main") }.unwrap() as u64);
        }

        Some("repl") | None => repl(),

        _ => unreachable!("Invalid subcommand"),
    }
}

fn check(s: &str, mod_name: &str, root: &mut frontend_ir::Ir) {
    let ast = match parser::parse(s) {
        Ok(v) => v,

        Err(_) => {
            eprintln!("Error parsing!");
            exit(1);
        }
    };

    match frontend_ir::convert_ast_to_ir(mod_name, &s, ast, root) {
        Ok(v) => v,
        Err(_) => {
            eprintln!("Error creating ir!");
            exit(1);
        }
    };

    let _ = correctness::check_correctness(root, true);
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

fn repl() {
    let mut rl = Editor::<()>::new();
    let mut root = frontend_ir::Ir::new();
    let mut i = 0;

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);

                let mod_name = format!("m{}", i);
                i += 1;
                check(&line, &mod_name, &mut root);
                let f_module = root.modules.get(&mod_name).unwrap();

                let mut b_module = backend_ir::convert_frontend_ir_to_backend_ir(f_module);

                let code = match compile(&mut b_module) {
                    Some(v) => v,
                    None => return,
                };

                let jit = Jit::new(code);
                println!(
                    "{:#x}",
                    unsafe { jit.call(f_module.funcs.iter().next().unwrap().0) }.unwrap() as u64
                );
            }

            Err(ReadlineError::Interrupted) => {
                println!("^C");
            }

            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }

            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }
}
