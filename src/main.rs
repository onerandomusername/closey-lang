use std::env;

pub static DEBUG: bool = false;

fn main() {
    let mut args = env::args();
    let name = args.next().unwrap();

    if args.len() == 1 && args.peekable().peek().unwrap() == "help" {
        println!(
            "usage:
{}                          - opens up the repl
{} check [files]            - checks the given files for correctness
{} build [options] [files]  - builds the given files
{} run [options] [files]    - runs the given files",
            name, name, name, name
        );
    } else {
        println!("uwu");
    }
}

