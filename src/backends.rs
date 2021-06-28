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

use ir::IrFunction;

pub trait Code {
    /// Gets the length of the x86 code.
    fn len(&self) -> usize;

    /// Returns true if the code is empty.
    fn is_empty(&self) -> bool;

    /// Returns the code as a pointer.
    fn as_ptr(&self) -> *const u8;

    /// Relocates all function addresses to their offset plus the base pointer provided.
    fn relocate(&mut self, base: *const u8);

    /// Returns executable code as a function.
    ///
    /// # Safety
    /// This function uses transmute to turn a pointer to raw bytes into a function, so use it with
    /// caution.
    unsafe fn get_fn(
        &self,
        func: &str,
        base: *const u8,
    ) -> Option<unsafe extern "C" fn() -> u64>;

    /// Disassembles the machine code into human readable assembly to stdout.
    fn disassemble(&self, base: *const u8);
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
