/// Module for correctness checking. This module contains all the functions that are involved in,
/// for example, determining arity and type checking.
pub mod correctness;

/// Module for the frontend intermediate representation. This module contains functions for
/// generating the IR and handling it.
pub mod ir;

/// Module for parsing the source text.
pub mod parser;

/// Module for scopes. This module contains functions for manipulating scopes and variables.
pub mod scopes;

/// Module for types. This module contains functions to help with type checking and manipulating
/// types.
pub mod types;
