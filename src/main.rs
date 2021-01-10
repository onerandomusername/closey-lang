use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::collections::HashMap;
use std::env;
use std::fs;

use curly_lang::frontend::correctness;
use curly_lang::frontend::ir;
use curly_lang::frontend::ir::IR;
use curly_lang::frontend::parser;

fn main()
{
    let args = env::args();
    println!("{:?}", &args);

    if args.len() == 1
    {
        repl();
    } else
    {
        let args: Vec<String> = args.into_iter().collect();
        exec_file(&args[1]);
    }
}

// exec_file(&str) -> ()
// Executes a file.
fn exec_file(file: &str)
{
    // Get file contents
    let contents = match fs::read_to_string(file)
    {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    // Execute file
    let mut ir = IR::new();
    execute(&contents, &mut ir);
}

// repl() -> ()
// Executes the REPL.
fn repl()
{
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err()
    {
        println!("No previous history.");
    }

    let mut ir = IR::new();

    loop
    {
        // Get line
        let readline = rl.readline(">>> ");
        match readline
        {
            Ok(line) => {
                println!("Line: {}", line);

                // Quitting
                if line == ":q" || line == ":quit"
                {
                    break;
                }

                rl.add_history_entry(line.as_str());
                execute(&line, &mut ir);
            }

            // Errors
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }

            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }

            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history("history.txt").unwrap();
}

// execute(&str, &mut IR) -> ()
// Executes Curly code.
fn execute(code: &str, ir: &mut IR)
{
    use std::mem::swap;

    // Generate the ast
    let ast = match parser::parse(code)
    {
        Ok(v) => v,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    // Print out the ast
    println!("{:#?}", &ast);
    ir.clear();
    ir::convert_ast_to_ir(ast, ir);
    println!("{:#?}", &ir);

    // Check correctness
    let err = correctness::check_correctness(ir);

    // Print out the ir or the error
    match err
    {
        Ok(_) => println!("{:#?}", &ir),
        Err(e) => println!("{:?}", e)
    }

    // Filter out nonglobal functions
    let mut temp = HashMap::with_capacity(0);
    swap(&mut ir.funcs, &mut temp);
    let mut temp = temp.into_iter().filter(|v| v.1.global).collect();
    swap(&mut ir.funcs, &mut temp);
}

