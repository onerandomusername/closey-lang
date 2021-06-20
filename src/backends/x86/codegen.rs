use std::collections::HashMap;

use super::super::ir::{IrArgument, IrFunction, IrInstruction, IrModule};

const NONARG_REGISTER_COUNT: usize = 9;

#[derive(Copy, Clone)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Spilled
}

impl Register {
    fn convert_nonarg_register_id(id: usize) -> Register {
        use Register::*;

        // rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
        match id {
            0 => RAX,
            1 => RDI,
            2 => RSI,
            3 => RDX,
            4 => RCX,
            5 => R8,
            6 => R9,
            7 => R10,
            8 => R11,
            _ => Spilled
        }
    }

    fn convert_to_instr_arg(&self) -> (bool, u8) {
        use Register::*;

        match self {
            RAX => (false, 0),
            RCX => (false, 1),
            RDX => (false, 2),
            RBX => (false, 3),
            RSP => (false, 4),
            RBP => (false, 5),
            RSI => (false, 6),
            RDI => (false, 7),
            R8 => (true, 0),
            R9 => (true, 1),
            R10 => (true, 2),
            R11 => (true, 3),
            R12 => (true, 4),
            R13 => (true, 5),
            R14 => (true, 6),
            R15 => (true, 7),
            Spilled => todo!()
        }
    }
}

#[derive(Default)]
pub struct GeneratedCode {
    func_addrs: HashMap<String, usize>,
    func_refs: HashMap<usize, String>,
    data: Vec<u8>
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

    pub fn relocate(&mut self, _base: *const u8) {
        todo!();
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

fn linear_scan(func: &mut IrFunction, register_count: usize) {
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
            ssa.local_register = register_count;
        }
    }
}

pub fn generate_code(module: &mut IrModule) -> GeneratedCode {
    let mut code = GeneratedCode::new();

    for func in module.funcs.iter_mut() {
        // Add padding
        while code.data.len() % 16 != 0 {
            code.data.push(0);
        }

        // Put arity just before function
        code.data.push((func.argc         & 0xff) as u8);
        code.data.push(((func.argc >>  8) & 0xff) as u8);
        code.data.push(((func.argc >> 16) & 0xff) as u8);
        code.data.push(((func.argc >> 24) & 0xff) as u8);
        if std::mem::size_of::<usize>() == 8 {
            code.data.push(((func.argc >> 32) & 0xff) as u8);
            code.data.push(((func.argc >> 40) & 0xff) as u8);
            code.data.push(((func.argc >> 48) & 0xff) as u8);
            code.data.push(((func.argc >> 56) & 0xff) as u8);
        } else {
            code.data.push(0);
            code.data.push(0);
            code.data.push(0);
            code.data.push(0);
        }

        // More padding
        code.data.push(0);
        code.data.push(0);
        code.data.push(0);
        code.data.push(0);
        code.data.push(0);
        code.data.push(0);
        code.data.push(0);
        code.data.push(0);

        // Add function
        code.func_addrs.insert(func.name.clone(), code.len());

        linear_scan(func, NONARG_REGISTER_COUNT);

        let mut local_to_register = HashMap::new();
        for ssa in func.ssas.iter() {
            if let Some(local) = ssa.local {
                local_to_register.insert(local, Register::convert_nonarg_register_id(ssa.local_register));
            }

            match ssa.instr {
                IrInstruction::Ret => {
                    if let Some(IrArgument::Local(arg)) = ssa.args.first() {
                        let (reg64, register) = local_to_register.get(arg).unwrap().convert_to_instr_arg();

                        // mov %rax, arg0
                        code.data.push(0x48 | if reg64 { 1 } else { 0 });
                        code.data.push(0x89);
                        code.data.push(0xc0 | (register << 3));
                    }

                    // ret
                    code.data.push(0xc3);
                }

                IrInstruction::Load => {
                }

                IrInstruction::Func => {
                    if let Some(local) = ssa.local {
                        if let Some(IrArgument::Function(func)) = ssa.args.first() {
                            let (reg64, register) = local_to_register.get(&local).unwrap().convert_to_instr_arg();

                            // mov reg, func
                            code.data.push(0x48 | if reg64 { 1 } else { 0 });
                            code.data.push(0xb8 | register);

                            // Value
                            for _ in 0..8 {
                                code.data.push(0x69);
                            }
                        }
                    }
                }

                IrInstruction::Call => todo!(),
                IrInstruction::Apply => todo!(),
            }
        }
    }

    code
}

