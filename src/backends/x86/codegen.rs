use std::collections::HashMap;

use super::super::ir::{IrArgument, IrInstruction, IrModule};
use super::super::common::{self, GeneratedCode};

const ARG_REGISTER_COUNT: usize = 6;
const NONARG_REGISTER_COUNT: usize = 9;

enum InstructionRegister {
    Bit32(u8),
    Bit64(u8),
    Spilled(usize),
    Arg(usize)
}

impl InstructionRegister {
    fn is_register(&self) -> bool {
        match self {
            Self::Bit32(_)
                | Self::Bit64(_) => true,

            Self::Spilled(_)
                | Self::Arg(_) => false
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
            Self::Bit32(r)
                | Self::Bit64(r) => *r,

            Self::Spilled(_) => panic!("Spilled values are not registers!"),
            Self::Arg(_) => panic!("Argument values are not registers!")
        }
    }

    fn get_offset(&self) -> usize {
        match self {
            Self::Spilled(v)
                | Self::Arg(v) => *v,

            Self::Bit32(_)
                | Self::Bit64(_) => panic!("Register cannot be an offset!")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Register {
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
    Spilled(usize),
    Arg(usize)
}

impl Register {
    fn convert_arg_register_id(id: usize) -> Register {
        use Register::*;

        match id {
            0 => RDI,
            1 => RSI,
            2 => RDX,
            3 => RCX,
            4 => R8,
            5 => R9,
            _ => Arg(id - ARG_REGISTER_COUNT)
        }
    }

    fn convert_nonarg_register_id(id: usize) -> Register {
        use Register::*;

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
            _ => Spilled(id - NONARG_REGISTER_COUNT)
        }
    }

    fn convert_to_instr_arg(&self) -> InstructionRegister {
        use Register::*;
        use InstructionRegister as IR;

        match self {
            RAX => IR::Bit32(0),
            RCX => IR::Bit32(1),
            RDX => IR::Bit32(2),
            RBX => IR::Bit32(3),
            RSP => IR::Bit32(4),
            RBP => IR::Bit32(5),
            RSI => IR::Bit32(6),
            RDI => IR::Bit32(7),
            R8  => IR::Bit64(0),
            R9  => IR::Bit64(1),
            R10 => IR::Bit64(2),
            R11 => IR::Bit64(3),
            R12 => IR::Bit64(4),
            R13 => IR::Bit64(5),
            R14 => IR::Bit64(6),
            R15 => IR::Bit64(7),
            Spilled(s) => IR::Spilled(*s),
            Arg(s) => IR::Arg(*s)
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

        common::linear_scan(func, NONARG_REGISTER_COUNT);

        let mut local_to_register = HashMap::new();
        for ssa in func.ssas.iter() {
            if let Some(local) = ssa.local {
                local_to_register.insert(local, Register::convert_nonarg_register_id(ssa.local_register));
            }

            match ssa.instr {
                IrInstruction::Ret => {
                    if let Some(IrArgument::Local(arg)) = ssa.args.first() {
                        let register = local_to_register.get(arg).unwrap();
                        if *register != Register::RAX {
                            let local_location = local_to_register.get(arg).unwrap().convert_to_instr_arg();

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

                                let offset: u32 = u32::MAX - (local_location.get_offset() as u32 - 1) * 8;
                                code.data.push((offset         & 0xff) as u8);
                                code.data.push(((offset >>  8) & 0xff) as u8);
                                code.data.push(((offset >> 16) & 0xff) as u8);
                                code.data.push(((offset >> 24) & 0xff) as u8);
                            }
                        }
                    }

                    // ret
                    code.data.push(0xc3);
                }

                IrInstruction::Load => {
                    if let Some(local) = ssa.local {
                        let local_location = local_to_register.get(&local).unwrap().convert_to_instr_arg();

                        match ssa.args.first() {
                            Some(IrArgument::Argument(arg)) => {
                                let arg_location = Register::convert_arg_register_id(*arg).convert_to_instr_arg();

                                // mov local, arg
                                match (arg_location.is_register(), local_location.is_register()) {
                                    (true, true) => {
                                        code.data.push(0x48 | arg_location.is_64_bit() | (local_location.is_64_bit() << 2));
                                        code.data.push(0x89);
                                        code.data.push(0xc0 | (arg_location.get_register() << 3) | local_location.get_register());
                                    }

                                    (true, false) => {
                                        todo!();
                                    }

                                    (false, true) => {
                                        // TODO: check this (im pretty sure its correct though)
                                        // mov local, [rbp + offset]
                                        code.data.push(0x48 | (local_location.is_64_bit() << 2));
                                        code.data.push(0x8b);
                                        code.data.push(0x85 | (local_location.get_register() << 3));

                                        let offset: u32 = u32::MAX - (local_location.get_offset() as u32 - 1) * 8;
                                        code.data.push((offset         & 0xff) as u8);
                                        code.data.push(((offset >>  8) & 0xff) as u8);
                                        code.data.push(((offset >> 16) & 0xff) as u8);
                                        code.data.push(((offset >> 24) & 0xff) as u8);
                                    }

                                    (false, false) => {
                                        todo!();
                                    }
                                }
                            }

                            Some(IrArgument::Function(func)) => {
                                // mov local, func
                                code.data.push(0x48 | local_location.is_64_bit());
                                if local_location.is_register() {
                                    code.data.push(0xb8 | local_location.get_register());
                                } else {
                                    // todo!();
                                }

                                // Insert the label
                                code.func_refs.insert(code.data.len(), func.clone());

                                // Value
                                for _ in 0..8 {
                                    code.data.push(0);
                                }
                            }

                            _ => ()
                        }
                    }
                }

                IrInstruction::Apply => todo!(),
                IrInstruction::Call(_known_arity) => {
                    /*
                    if known_arity {
                        for (i, arg) in ssa.args.iter().skip(1).enumerate() {
                            let reg = Register::convert_arg_register_id(i);
                        }

                        // call register
                        let (reg64, func_register) = local_to_register.get(if let IrArgument::Local(func) = ssa.args.first().unwrap() { func } else { unreachable!(); }).unwrap().convert_to_instr_arg();
                        if reg64 {
                            code.data.push(0x41);
                        }
                        code.data.push(0xff);
                        code.data.push(0xd0 | func_register)
                    } else {
                        todo!();
                    }
                    */
                }
            }
        }
    }

    code
}

