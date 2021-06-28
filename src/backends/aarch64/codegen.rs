use std::collections::HashMap;
use std::ops::Range;

use super::super::Code;
use super::super::ir::IrModule;

/// Represents generated aarch64 code.
#[derive(Default)]
pub struct GeneratedCode {
    func_addrs: HashMap<String, Range<usize>>,
    func_refs: HashMap<usize, (String, bool)>,
    data: Vec<u8>,
}

impl GeneratedCode {
    fn new() -> GeneratedCode {
        GeneratedCode {
            func_addrs: HashMap::new(),
            func_refs: HashMap::new(),
            data: Vec::new(),
        }
    }
}

impl Code for GeneratedCode {
    /// Gets the length of the x86 code.
    fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns true if the code is empty.
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the code as a pointer.
    fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    /// Relocates all function addresses to their offset plus the base pointer provided.
    fn relocate(&mut self, base: *const u8) {
        for (code_addr, (func, relative)) in self.func_refs.iter() {
            if let Some(range) = self.func_addrs.get(func) {
                let (addr, byte_count) = if *relative {
                    ((range.start as i32 - *code_addr as i32 - 4) as u64, 4)
                } else {
                    (base as u64 + range.start as u64, 8)
                };

                for (i, byte) in self.data.iter_mut().skip(*code_addr).enumerate() {
                    if i >= byte_count {
                        break;
                    }

                    *byte = ((addr >> (i * 8)) & 0xff) as u8;
                }
            }
        }
    }

    /// Returns executable code as a function.
    ///
    /// # Safety
    /// This function uses transmute to turn a pointer to raw bytes into a function, so use it with
    /// caution.
    unsafe fn get_fn(
        &self,
        func: &str,
        base: *const u8,
    ) -> Option<unsafe extern "C" fn() -> u64> {
        if let Some(f) = self.func_addrs.get(func) {
            use std::mem::transmute;
            Some(transmute(base.add(f.start)))
        } else {
            None
        }
    }

    /// Disassembles the machine code into human readable assembly to stdout.
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    fn disassemble(&self, _: *const u8) {
        todo!();
    }
}

/// Transforms an IrModule into aarch64 machine code.
pub fn generate_code(_: &mut IrModule) -> GeneratedCode {
    let mut code = GeneratedCode::new();
    code.data = vec![0xd1, 0x00, 0x43, 0xff, 0xb9, 0x00, 0x0f, 0xff, 0x52, 0x80, 0x08, 0xa0, 0x91, 0x00, 0x43, 0xff, 0xd6, 0x5f, 0x03, 0xc0];
    code.func_addrs.insert(String::from("main"), 0..code.len());
    code
}
