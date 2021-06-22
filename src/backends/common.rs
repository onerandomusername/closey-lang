use std::collections::HashMap;

use super::ir::IrFunction;

#[derive(Default)]
pub struct GeneratedCode {
    pub(crate) func_addrs: HashMap<String, usize>,
    pub(crate) func_refs: HashMap<usize, String>,
    pub(crate) data: Vec<u8>
}

impl GeneratedCode {
    pub fn new() -> GeneratedCode {
        GeneratedCode {
            func_addrs: HashMap::new(),
            func_refs: HashMap::new(),
            data: Vec::new()
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn relocate(&mut self, base: *const u8) {
        for (code_addr, func) in self.func_refs.iter() {
            if let Some(offset) = self.func_addrs.get(func) {
                let addr = base as usize + *offset;

                for (i, byte) in self.data.iter_mut().skip(*code_addr).enumerate() {
                    if i >= 8 {
                        break;
                    }

                    *byte = ((addr >> (i * 8)) & 0xff) as u8;
                }
            }
        }
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn get_main_fn(&self, base: *const u8) -> Option<extern "C" fn() -> u64> {
        if let Some(f) = self.func_addrs.get("main") {
            use std::mem::transmute;
            Some(transmute(base.add(*f)))
        } else {
            None
        }
    }

    pub fn print_data(&self) {
        let mut i = 0;
        for c in self.data.iter() {
            print!("{:02x} ", c);
            i += 1;
            if i >= 16 {
                i = 0;
                println!();
            }
        }
        if i != 0 {
            println!();
        }
    }
}

pub(crate) fn linear_scan(func: &mut IrFunction, register_count: usize) {
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

