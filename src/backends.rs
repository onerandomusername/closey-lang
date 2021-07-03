/// Module for the lower level intermediate representation. This module contains functions that
/// generate the IR and manipulate it.
pub mod ir;

/// Module for aarch64 code generation.
pub mod aarch64;

/// Module for RISC-V code generation.
pub mod riscv64;

/// Module for x86_64 code generation.
pub mod x86_64;

/// Module for wasm64 code generation.
pub mod wasm64;

use std::collections::HashMap;
use std::ops::Range;

use ir::IrFunction;

#[cfg(target_arch = "aarch64")]
pub const DEFAULT_ARCH: &str = "aarch64";
#[cfg(target_arch = "riscv64")]
pub const DEFAULT_ARCH: &str = "riscv64";
#[cfg(target_arch = "wasm64")]
pub const DEFAULT_ARCH: &str = "wasm64";
#[cfg(target_arch = "x86_64")]
pub const DEFAULT_ARCH: &str = "x86_64";

#[cfg(target_os = "linux")]
pub const DEFAULT_OS: &str = "linux";
#[cfg(target_os = "macos")]
pub const DEFAULT_OS: &str = "macos";

/// Represents generated code in some architecture.
#[derive(Default)]
pub struct GeneratedCode {
    func_addrs: HashMap<String, Range<usize>>,
    func_refs: HashMap<usize, String>,
    data: Vec<u8>,
}

impl GeneratedCode {
    pub(crate) fn new() -> GeneratedCode {
        GeneratedCode {
            func_addrs: HashMap::new(),
            func_refs: HashMap::new(),
            data: Vec::new(),
        }
    }

    /// Gets the length of the x86 code.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns true if the code is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the code as a Vec.
    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }

    /// Returns executable code as a function.
    ///
    /// # Safety
    /// This function uses transmute to turn a pointer to raw bytes into a function, so use it with
    /// caution.
    pub unsafe fn get_fn(
        &self,
        func: &str,
        base: *const u8,
    ) -> Option<unsafe extern "C" fn() -> *const u8> {
        if let Some(f) = self.func_addrs.get(func) {
            use std::mem::transmute;
            Some(transmute(base.add(f.start)))
        } else {
            None
        }
    }

    /// Gets the mapping from function names to ranges in code.
    pub fn get_funcs(&self) -> &HashMap<String, Range<usize>> {
        &self.func_addrs
    }

    /// Gets the mapping used to relocate a file.
    pub fn get_relocation_table(&self) -> &HashMap<usize, String> {
        &self.func_refs
    }
}

/// Performs register allocation by linear scan on an IrFunction.
pub fn linear_scan(func: &mut IrFunction, register_count: usize) {
    let mut register_lifetimes = vec![0usize; register_count];

    'a: for ssa in func.ssas.iter_mut() {
        for lifetime in register_lifetimes.iter_mut() {
            if *lifetime > 0 {
                *lifetime -= 1;
            }
        }

        if ssa.local.is_some() {
            for (reg, lifetime) in register_lifetimes.iter_mut().enumerate() {
                if *lifetime == 0 {
                    *lifetime = ssa.local_lifetime;
                    ssa.local_register = reg;
                    continue 'a;
                }
            }
            ssa.local_register = register_lifetimes.len();
            register_lifetimes.push(ssa.local_lifetime);
        }
    }
}
