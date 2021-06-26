/// Module for common backend functions.
pub mod common;

/// Module for the lower level intermediate representation. This module contains functions that
/// generate the IR and manipulate it.
pub mod ir;

/// Module for arm code generation.
pub mod arm;

/// Module for RISC-V code generation.
pub mod riscv;

/// Module for x86_64 code generation.
pub mod x86_64;

/// Module for wasm code generation.
pub mod wasm;
