# Closey
Closey is a functional programming language designed to have as few features as possible but still be easy to use and compiles to efficient machine code.

## Features
- Functions
- Function applications
- Partial function application
- Union types
- Match expressions
- Closures
- Closures are tagged with the type of the closed over values
- Monomorphism
- Optimisation of church numerals and cons boxes to corresponding native types

## Examples
### Identity
```ocaml
(\a: 'a . a) (\a: 'a . a)
```

## Build
Just type in the following:
```bash
git clone https://github.com/jenra-uwu/closey-lang && cd closey-lang && cargo build
```

Note: This repo has only been tested on Arch Linux, but should work on all other platforms rust supports without much issue.

## Progress
See TODO.md. Everything is highly experimental. Be cautious: code may be explosive.

## Support
Come to the [official discord server!](https://discord.gg/Gxfr6JDecv)

