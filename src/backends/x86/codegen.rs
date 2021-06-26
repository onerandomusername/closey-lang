use std::collections::HashMap;

use super::super::common;
use super::super::ir::{IrArgument, IrInstruction, IrModule};

const ARG_REGISTER_COUNT: usize = 6;
const NONARG_REGISTER_COUNT: usize = 8;

enum InstructionRegister {
    Bit32(u8),
    Bit64(u8),
    Spilled(usize),
    Arg(usize),
}

impl InstructionRegister {
    fn is_register(&self) -> bool {
        match self {
            Self::Bit32(_) | Self::Bit64(_) => true,

            Self::Spilled(_) | Self::Arg(_) => false,
        }
    }

    fn is_64_bit(&self) -> u8 {
        if let Self::Bit64(_) = self {
            1
        } else {
            0
        }
    }

    fn get_register(&self) -> u8 {
        match self {
            Self::Bit32(r) | Self::Bit64(r) => *r,

            Self::Spilled(_) => panic!("Spilled values are not registers!"),
            Self::Arg(_) => panic!("Argument values are not registers!"),
        }
    }

    fn get_offset(&self) -> usize {
        match self {
            Self::Spilled(v) | Self::Arg(v) => *v,

            Self::Bit32(_) | Self::Bit64(_) => panic!("Register cannot be an offset!"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
enum Register {
    Rax, // scratch and return register
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Spilled(usize),
    Arg(usize),
}

impl Register {
    fn convert_arg_register_id(id: usize) -> Register {
        use Register::*;

        match id {
            0 => Rdi,
            1 => Rsi,
            2 => Rdx,
            3 => Rcx,
            4 => R8,
            5 => R9,
            _ => Arg(id - ARG_REGISTER_COUNT),
        }
    }

    fn convert_nonarg_register_id(id: usize) -> Register {
        use Register::*;

        match id {
            0 => Rbx,
            1 => Rdx,
            2 => R10,
            3 => R11,
            4 => R12,
            5 => R13,
            6 => R14,
            7 => R15,
            _ => Spilled(id - NONARG_REGISTER_COUNT + 1),
        }
    }

    fn convert_to_instr_arg(&self) -> InstructionRegister {
        use InstructionRegister as IR;
        use Register::*;

        match self {
            Rax => IR::Bit32(0),
            Rcx => IR::Bit32(1),
            Rdx => IR::Bit32(2),
            Rbx => IR::Bit32(3),
            Rsp => IR::Bit32(4),
            Rbp => IR::Bit32(5),
            Rsi => IR::Bit32(6),
            Rdi => IR::Bit32(7),
            R8 => IR::Bit64(0),
            R9 => IR::Bit64(1),
            R10 => IR::Bit64(2),
            R11 => IR::Bit64(3),
            R12 => IR::Bit64(4),
            R13 => IR::Bit64(5),
            R14 => IR::Bit64(6),
            R15 => IR::Bit64(7),
            Spilled(s) => IR::Spilled(*s),
            Arg(s) => IR::Arg(*s),
        }
    }
}

#[derive(Default)]
pub struct GeneratedCode {
    pub(crate) func_addrs: HashMap<String, usize>,
    pub(crate) func_refs: HashMap<usize, (String, bool)>,
    pub(crate) data: Vec<u8>,
}

impl GeneratedCode {
    fn new() -> GeneratedCode {
        GeneratedCode {
            func_addrs: HashMap::new(),
            func_refs: HashMap::new(),
            data: Vec::new(),
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
        for (code_addr, (func, relative)) in self.func_refs.iter() {
            if let Some(offset) = self.func_addrs.get(func) {
                let (addr, byte_count) = if *relative {
                    ((*offset as i32 - *code_addr as i32 - 4) as u64, 4)
                } else {
                    (base as u64 + *offset as u64, 8)
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

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn get_fn(&self, func: &str, base: *const u8) -> Option<extern "C" fn() -> u64> {
        if let Some(f) = self.func_addrs.get(func) {
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

pub fn generate_code(module: &mut IrModule) -> GeneratedCode {
    let mut code = GeneratedCode::new();

    for func in module.funcs.iter_mut() {
        // Add padding
        while code.data.len() % 16 != 0 {
            code.data.push(0);
        }

        // Put arity just before function
        code.data.push((func.argc & 0xff) as u8);
        code.data.push(((func.argc >> 8) & 0xff) as u8);
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

        // push rbp
        code.data.push(0x55);

        // mov rbp, rsp
        code.data.push(0x48);
        code.data.push(0x89);
        code.data.push(0xe5);

        common::linear_scan(func, NONARG_REGISTER_COUNT);

        let mut local_to_register = HashMap::new();
        for ssa in func.ssas.iter() {
            if let Some(local) = ssa.local {
                local_to_register.insert(
                    local,
                    Register::convert_nonarg_register_id(ssa.local_register),
                );
            }

            match ssa.instr {
                IrInstruction::Ret => {
                    if let Some(IrArgument::Local(arg)) = ssa.args.first() {
                        let register = local_to_register.get(arg).unwrap();
                        if *register != Register::Rax {
                            let local_location =
                                local_to_register.get(arg).unwrap().convert_to_instr_arg();

                            if local_location.is_register() {
                                // mov rax, local_reg
                                code.data.push(0x48 | local_location.is_64_bit());
                                code.data.push(0x89);
                                code.data.push(0xc0 | (local_location.get_register() << 3));
                            } else {
                                // TODO: check this (im pretty sure its correct though)
                                // mov rax, [rbp + offset]
                                code.data.push(0x48);
                                code.data.push(0x8b);
                                code.data.push(0x85);

                                let offset: u32 =
                                    (-(local_location.get_offset() as i32) * 8) as u32;
                                code.data.push((offset & 0xff) as u8);
                                code.data.push(((offset >> 8) & 0xff) as u8);
                                code.data.push(((offset >> 16) & 0xff) as u8);
                                code.data.push(((offset >> 24) & 0xff) as u8);
                            }
                        }
                    }

                    // mov rsp, rbp
                    code.data.push(0x48);
                    code.data.push(0x89);
                    code.data.push(0xec);

                    // pop rbp
                    code.data.push(0x5d);

                    // ret
                    code.data.push(0xc3);
                }

                IrInstruction::Load => {
                    if let Some(local) = ssa.local {
                        let local_location = local_to_register
                            .get(&local)
                            .unwrap()
                            .convert_to_instr_arg();

                        match ssa.args.first() {
                            Some(IrArgument::Argument(arg)) => {
                                let arg_location =
                                    Register::convert_arg_register_id(*arg).convert_to_instr_arg();

                                match (local_location.is_register(), arg_location.is_register()) {
                                    (true, true) => {
                                        // mov local, arg
                                        code.data.push(
                                            0x48 | arg_location.is_64_bit()
                                                | (local_location.is_64_bit() << 2),
                                        );
                                        code.data.push(0x89);
                                        code.data.push(
                                            0xc0 | (arg_location.get_register() << 3)
                                                | local_location.get_register(),
                                        );
                                    }

                                    (false, true) => {
                                        // mov [rbp - offset], arg
                                        todo!();
                                    }

                                    (true, false) => {
                                        // TODO: check this (im pretty sure its correct though)
                                        // mov local, [rbp + offset]
                                        code.data.push(0x48 | (local_location.is_64_bit() << 2));
                                        code.data.push(0x8b);
                                        code.data.push(
                                            0x80 | (local_location.get_register() << 3)
                                                | Register::Rbp
                                                    .convert_to_instr_arg()
                                                    .get_register(),
                                        );

                                        let offset: u32 =
                                            ((arg_location.get_offset() as i32 + 2) * 8) as u32;
                                        code.data.push((offset & 0xff) as u8);
                                        code.data.push(((offset >> 8) & 0xff) as u8);
                                        code.data.push(((offset >> 16) & 0xff) as u8);
                                        code.data.push(((offset >> 24) & 0xff) as u8);
                                    }

                                    (false, false) => {
                                        // mov rax, [rbp + offset]
                                        // mov [rbp - offset], rax
                                        todo!();
                                    }
                                }
                            }

                            Some(IrArgument::Function(func)) => {
                                if local_location.is_register() {
                                    // mov local, func
                                    code.data.push(0x48 | local_location.is_64_bit());
                                    code.data.push(0xb8 | local_location.get_register());

                                    // Insert the label
                                    code.func_refs
                                        .insert(code.data.len(), (func.clone(), false));

                                    // Value
                                    for _ in 0..8 {
                                        code.data.push(0);
                                    }
                                } else {
                                    todo!();
                                }
                            }

                            _ => (),
                        }
                    }
                }

                IrInstruction::Apply => todo!(),
                IrInstruction::Call(known_arity) => {
                    if known_arity {
                        // First 6 arguments are stored in registers
                        for (i, arg) in ssa.args.iter().skip(1).enumerate() {
                            let reg = Register::convert_arg_register_id(i).convert_to_instr_arg();

                            match arg {
                                IrArgument::Local(_) => todo!(),
                                IrArgument::Argument(_) => todo!(),

                                IrArgument::Function(func) => {
                                    // mov reg, func
                                    code.data.push(0x48 | reg.is_64_bit());
                                    code.data.push(0xb8 | reg.get_register());

                                    // Insert the label
                                    code.func_refs
                                        .insert(code.data.len(), (func.clone(), false));

                                    // Value
                                    for _ in 0..8 {
                                        code.data.push(0);
                                    }
                                }
                            }

                            if i == ARG_REGISTER_COUNT - 1 {
                                break;
                            }
                        }

                        // Rest of the registers are stored on the stack
                        for arg in ssa.args.iter().skip(ARG_REGISTER_COUNT + 1).rev() {
                            match arg {
                                IrArgument::Local(_) => todo!(),
                                IrArgument::Argument(_) => todo!(),

                                IrArgument::Function(func) => {
                                    // mov rax, func
                                    code.data.push(0x48);
                                    code.data.push(0xb8);

                                    // Insert the label
                                    code.func_refs
                                        .insert(code.data.len(), (func.clone(), false));

                                    // Value
                                    for _ in 0..8 {
                                        code.data.push(0);
                                    }

                                    // push rax
                                    code.data.push(0x50);
                                }
                            }
                        }

                        match ssa.args.first().unwrap() {
                            IrArgument::Local(_) => todo!(),
                            IrArgument::Argument(_) => todo!(),

                            IrArgument::Function(func) => {
                                // call func
                                code.data.push(0xe8);

                                // Insert the label
                                code.func_refs.insert(code.data.len(), (func.clone(), true));

                                // Value
                                for _ in 0..4 {
                                    code.data.push(0);
                                }
                            }
                        }

                        if let Some(local) = ssa.local {
                            let local_location = Register::convert_nonarg_register_id(local)
                                .convert_to_instr_arg();

                            if local_location.is_register() {
                                // mov local, rax
                                code.data.push(0x48 | local_location.is_64_bit());
                                code.data.push(0x8b);
                                code.data.push(
                                    0xc0 | (local_location.get_register() << 3)
                                        | Register::Rax.convert_to_instr_arg().get_register(),
                                );
                            } else {
                                todo!();
                            }
                        }
                    } else {
                        todo!();
                    }
                }
            }
        }
    }

    code
}
