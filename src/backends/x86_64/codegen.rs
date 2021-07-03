use std::collections::{HashMap, HashSet};

use super::super::super::backends;
use super::super::ir::{IrArgument, IrInstruction, IrModule};
use super::super::GeneratedCode;

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
            _ => Spilled(id - NONARG_REGISTER_COUNT),
        }
    }

    fn revert_to_nonarg_register_id(&self) -> usize {
        use Register::*;

        match self {
            Rbx => 0,
            Rdx => 1,
            R10 => 2,
            R11 => 3,
            R12 => 4,
            R13 => 5,
            R14 => 6,
            R15 => 7,
            Spilled(id) => id + NONARG_REGISTER_COUNT,
            _ => panic!("Arguments are not not arguments!"),
        }
    }

    fn is_callee_saved(&self) -> bool {
        use Register::*;
        matches!(self, Rbx | Rsp | Rbp | R12 | R13 | R14 | R15)
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
fn generate_mov(code: &mut GeneratedCode, dest: Register, source: Register) {
    let dest_location = dest.convert_to_instr_arg();
    let source_location = source.convert_to_instr_arg();

    match (dest_location.is_register(), source_location.is_register()) {
        (true, true) => {
            // mov dest_reg, source_reg
            code.data
                .push(0x48 | dest_location.is_64_bit() | (source_location.is_64_bit() << 2));
            code.data.push(0x89);
            code.data
                .push(0xc0 | dest_location.get_register() | (source_location.get_register() << 3));
        }

        (true, false) => {
            // mov rax, [rbp +- offset]
            code.data.push(0x48 | (dest_location.is_64_bit() << 2));
            code.data.push(0x8b);
            code.data.push(0x85 | (dest_location.get_register() << 3));

            let offset = if let InstructionRegister::Arg(a) = source_location {
                (a as u32 + 2) * 8
            } else if let InstructionRegister::Spilled(s) = source_location {
                (-(s as i32 + 1) * 8) as u32
            } else {
                unreachable!();
            };

            code.data.push((offset & 0xff) as u8);
            code.data.push(((offset >> 8) & 0xff) as u8);
            code.data.push(((offset >> 16) & 0xff) as u8);
            code.data.push(((offset >> 24) & 0xff) as u8);
        }

        (false, true) => {
            // mov [rbp +- offset], rax
            code.data.push(0x48 | (source_location.is_64_bit() << 2));
            code.data.push(0x89);
            code.data.push(0x85 | (source_location.get_register() << 3));

            let offset = if let InstructionRegister::Arg(a) = dest_location {
                (a as u32 + 2) * 8
            } else if let InstructionRegister::Spilled(s) = dest_location {
                (-(s as i32 + 1) * 8) as u32
            } else {
                unreachable!();
            };

            code.data.push((offset & 0xff) as u8);
            code.data.push(((offset >> 8) & 0xff) as u8);
            code.data.push(((offset >> 16) & 0xff) as u8);
            code.data.push(((offset >> 24) & 0xff) as u8);
        }

        (false, false) => {
            // mov rax, [rbp +- offset]
            generate_mov(code, Register::Rax, source);

            // mov [rbp +- offset], rax
            generate_mov(code, dest, Register::Rax);
        }
    }
}

fn generate_lea(code: &mut GeneratedCode, dest: Register, source: &str) {
    let dest_location = dest.convert_to_instr_arg();
    if dest_location.is_register() {
        code.data.push(0x48 | (dest_location.is_64_bit() << 2));
        code.data.push(0x8d);
        code.data.push(0x05 | (dest_location.get_register() << 3));
        code.func_refs.insert(code.data.len(), source.to_owned());
        code.data.push(0x01);
        code.data.push(0x00);
        code.data.push(0x00);
        code.data.push(0x00);
    } else {
        code.data.push(0x48);
        code.data.push(0x8d);
        code.data.push(0x05);
        code.func_refs.insert(code.data.len(), source.to_owned());
        code.data.push(0x01);
        code.data.push(0x00);
        code.data.push(0x00);
        code.data.push(0x00);
        generate_mov(code, dest, Register::Rax);
    }
}

/// Generates the _start function, which calls main and the exit syscall.
pub fn generate_start_func(code: &mut GeneratedCode) {
    code.func_addrs
        .insert(String::from("_start"), code.len()..code.len() + 1);
    code.func_addrs.insert(String::from("exit"), 0..0);

    // call main
    code.data.push(0xe8);
    code.func_refs
        .insert(code.len(), String::from("main"));
    code.data.push(0x00);
    code.data.push(0x00);
    code.data.push(0x00);
    code.data.push(0x00);

    // mov rdi, rax
    code.data.push(0x48);
    code.data.push(0x89);
    code.data.push(0xc7);

    // call exit
    code.data.push(0xe8);
    code.func_refs
        .insert(code.len(), String::from("exit"));
    code.data.push(0x00);
    code.data.push(0x00);
    code.data.push(0x00);
    code.data.push(0x00);

    code.func_addrs.get_mut("_start").unwrap().end = code.len();
}

/// Transforms an IrModule into x86 machine code.
pub fn generate_code(module: &mut IrModule) -> GeneratedCode {
    let mut code = GeneratedCode::new();

    for func in module.funcs.iter_mut() {
        // Add padding
        while code.data.len() % 16 != 0 {
            code.data.push(0);
        }

        // Add function
        code.func_addrs
            .insert(func.name.clone(), code.len()..code.len() + 1);

        // push rbp
        code.data.push(0x55);

        // mov rbp, rsp
        generate_mov(&mut code, Register::Rbp, Register::Rsp);

        backends::linear_scan(func, NONARG_REGISTER_COUNT);

        let mut used_registers = HashSet::new();
        for ssa in func.ssas.iter() {
            if ssa.local.is_some()
                && Register::convert_nonarg_register_id(ssa.local_register).is_callee_saved()
                && !used_registers.contains(&ssa.local_register)
            {
                used_registers.insert(ssa.local_register);
            }
        }

        // Push used registers
        let used_registers: Vec<_> = used_registers.into_iter().collect();
        for register in used_registers.iter() {
            let register = Register::convert_nonarg_register_id(*register).convert_to_instr_arg();
            if register.is_64_bit() != 0 {
                code.data.push(0x41);
            }
            code.data.push(0x50 | register.get_register());
        }

        let mut local_to_register = HashMap::new();
        let mut register_lifetimes = vec![0; NONARG_REGISTER_COUNT];
        let mut stack_allocated_local_count = 0usize;
        for ssa in func.ssas.iter() {
            for lifetime in register_lifetimes.iter_mut() {
                if *lifetime != 0 {
                    *lifetime -= 1;
                }
            }

            if let Some(local) = ssa.local {
                let register = Register::convert_nonarg_register_id(ssa.local_register);

                if register_lifetimes.len() < ssa.local_register {
                    register_lifetimes[ssa.local_register] = ssa.local_lifetime;
                } else {
                    register_lifetimes.push(ssa.local_lifetime);
                }

                local_to_register.insert(local, register);
            }

            match ssa.instr {
                IrInstruction::Ret => {
                    if let Some(IrArgument::Local(arg)) = ssa.args.first() {
                        let register = local_to_register.get(arg).unwrap();
                        generate_mov(&mut code, Register::Rax, *register);
                    }

                    // Pop used registers
                    for register in used_registers.iter().rev() {
                        let register =
                            Register::convert_nonarg_register_id(*register).convert_to_instr_arg();
                        if register.is_64_bit() != 0 {
                            code.data.push(0x41);
                        }
                        code.data.push(0x58 | register.get_register());
                    }

                    // mov rsp, rbp
                    generate_mov(&mut code, Register::Rsp, Register::Rbp);

                    // pop rbp
                    code.data.push(0x5d);

                    // ret
                    code.data.push(0xc3);
                }

                IrInstruction::Load => {
                    if let Some(local) = ssa.local {
                        let local_reg = *local_to_register.get(&local).unwrap();

                        match ssa.args.first() {
                            Some(IrArgument::Argument(arg)) => {
                                // mov local, [rbp + offset]
                                generate_mov(
                                    &mut code,
                                    local_reg,
                                    Register::convert_arg_register_id(*arg),
                                );
                            }

                            Some(IrArgument::Function(func)) => {
                                generate_lea(&mut code, local_reg, func);
                            }

                            _ => (),
                        }
                    }
                }

                IrInstruction::Apply => todo!(),

                IrInstruction::Call(known_arity) => {
                    if register_lifetimes[Register::R11.revert_to_nonarg_register_id()] != 0 {
                        // push r11
                        code.data.push(0x41);
                        code.data.push(0x53);
                    }

                    // Push arguments
                    for (i, _) in ssa.args.iter().skip(1).zip(0..func.argc).enumerate() {
                        let reg = Register::convert_arg_register_id(i).convert_to_instr_arg();
                        if !reg.is_register() {
                            break;
                        }

                        if reg.is_64_bit() != 0 {
                            code.data.push(0x41);
                        }

                        code.data.push(0x50 | reg.get_register());
                    }

                    if known_arity {
                        // First 6 arguments are stored in registers
                        for (i, arg) in ssa.args.iter().skip(1).enumerate() {
                            let arg_reg = Register::convert_arg_register_id(i);

                            match arg {
                                IrArgument::Local(local) => {
                                    let local_reg = *local_to_register.get(local).unwrap();

                                    // mov arg, local
                                    generate_mov(&mut code, arg_reg, local_reg);
                                }

                                IrArgument::Argument(arg) => {
                                    let local_reg = Register::convert_arg_register_id(*arg);

                                    // mov arg, local
                                    generate_mov(&mut code, arg_reg, local_reg);
                                }

                                IrArgument::Function(func) => {
                                    // lea arg, [rel func]
                                    generate_lea(&mut code, arg_reg, func);
                                }
                            }

                            if i == ARG_REGISTER_COUNT - 1 {
                                break;
                            }
                        }

                        // Rest of the registers are stored on the stack
                        for arg in ssa.args.iter().skip(ARG_REGISTER_COUNT + 1).rev() {
                            match arg {
                                IrArgument::Local(local) => {
                                    let local_reg = *local_to_register.get(local).unwrap();
                                    let local_location = local_reg.convert_to_instr_arg();

                                    if local_location.is_register() {
                                        // push local
                                        if local_location.is_64_bit() != 0 {
                                            code.data.push(0x41);
                                        }
                                        code.data.push(0x50 | local_location.get_register());
                                    } else {
                                        // mov rax, [rbp - offset]
                                        generate_mov(&mut code, Register::Rax, local_reg);

                                        // push rax
                                        code.data.push(0x50);
                                    }
                                }

                                IrArgument::Argument(_) => todo!(),

                                IrArgument::Function(func) => {
                                    // lea rax, [rel func]
                                    generate_lea(&mut code, Register::Rax, func);

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
                                code.func_refs.insert(code.data.len(), func.clone());

                                // Value
                                code.data.push(0x00);
                                code.data.push(0x00);
                                code.data.push(0x00);
                                code.data.push(0x00);
                            }
                        }
                    } else {
                        todo!();
                    }

                    // Pop arguments passed into the function and arguments saved
                    let mut pop_count = ssa.args.len() - 1
                        + std::cmp::min(
                            std::cmp::min(func.argc, ARG_REGISTER_COUNT),
                            ssa.args.len() - 1,
                        );
                    if pop_count > ARG_REGISTER_COUNT {
                        pop_count -= ARG_REGISTER_COUNT;
                    } else {
                        pop_count = 0;
                    }
                    pop_count *= 8;
                    if pop_count != 0 {
                        // sub rsp, pop_count
                        code.data.push(0x48);
                        code.data.push(0x81);
                        code.data.push(0xec);
                        code.data.push((pop_count & 0xff) as u8);
                        code.data.push(((pop_count >> 8) & 0xff) as u8);
                        code.data.push(((pop_count >> 16) & 0xff) as u8);
                        code.data.push(((pop_count >> 24) & 0xff) as u8);
                    }

                    if register_lifetimes[Register::R11.revert_to_nonarg_register_id()] != 0 {
                        // pop r11
                        code.data.push(0x41);
                        code.data.push(0x5b);
                    }

                    if let Some(local) = ssa.local {
                        let local_reg = Register::convert_nonarg_register_id(local);
                        let local_location = local_reg.convert_to_instr_arg();

                        if !local_location.is_register()
                            && local_location.get_offset() >= stack_allocated_local_count
                        {
                            stack_allocated_local_count += 1;

                            // push rax
                            code.data.push(0x50);
                        } else {
                            generate_mov(&mut code, local_reg, Register::Rax);
                        }
                    }
                }

                IrInstruction::RcInc => todo!(),

                IrInstruction::RcFuncFree => todo!(),
            }
        }
        code.func_addrs.get_mut(&func.name).unwrap().end = code.len();
    }

    code
}

/// Relocates all function addresses to their offset.
pub fn relocate(code: &mut GeneratedCode) {
    for (code_addr, func) in code.func_refs.iter() {
        if let Some(range) = code.func_addrs.get(func) {
            let addr = ((range.start as i32 - *code_addr as i32) as i64 + unsafe { *(code.data.as_ptr().add(*code_addr) as *const i32) } as i64 - 4) as u64;

            for (i, byte) in code.data.iter_mut().skip(*code_addr).enumerate() {
                if i >= 4 {
                    break;
                }

                *byte = ((addr >> (i * 8)) & 0xff) as u8;
            }
        }
    }
}

