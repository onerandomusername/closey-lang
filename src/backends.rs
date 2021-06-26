/// Module for common backend functions.
pub mod common;

/// Module for the lower level intermediate representation. This module contains functions that
/// generate the IR and manipulate it.
pub mod ir;

/// Module for x86_64 code generation.
pub mod x86;
