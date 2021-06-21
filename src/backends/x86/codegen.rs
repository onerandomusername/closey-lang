use std::collections::HashMap;

use super::super::ir::{IrArgument, IrInstruction, IrModule};
use super::super::common::{self, GeneratedCode};

const NONARG_REGISTER_COUNT: usize = 9;

#[derive(Copy, Clone, PartialEq, Eq)]
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
    fn convert_arg_register_id(id: usize) -> Register {
        use Register::*;

        match id {
            0 => RDI,
            1 => RSI,
            2 => RDX,
            3 => RCX,
            4 => R8,
            5 => R9,
            _ => Spilled
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
                            let (reg64, register) = local_to_register.get(arg).unwrap().convert_to_instr_arg();

                            // mov %rax, local
                            code.data.push(0x48 | if reg64 { 1 } else { 0 });
                            code.data.push(0x89);
                            code.data.push(0xc0 | (register << 3));
                        }
                    }

                    // ret
                    code.data.push(0xc3);
                }

                IrInstruction::Load => {
                    if let Some(local) = ssa.local {
                        let (reg64, local_register) = local_to_register.get(&local).unwrap().convert_to_instr_arg();

                        match ssa.args.first() {
                            Some(IrArgument::Argument(arg)) => {
                                let (mem64, arg_register) = Register::convert_arg_register_id(*arg).convert_to_instr_arg();

                                // mov local, arg
                                code.data.push(0x48 | if mem64 { 1 } else { 0 } | if reg64 { 4 } else { 0 });
                                code.data.push(0x89);
                                code.data.push(0xc0 | (arg_register << 3) | local_register);
                            }

                            Some(IrArgument::Function(func)) => {
                                // mov reg, func
                                code.data.push(0x48 | if reg64 { 1 } else { 0 });
                                code.data.push(0xb8 | local_register);

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

                IrInstruction::Call => todo!(),
                IrInstruction::Apply => todo!(),
            }
        }
    }

    code
}

