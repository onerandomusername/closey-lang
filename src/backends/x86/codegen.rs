use std::collections::{HashMap, HashSet};
use std::ops::Range;

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

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn get_fn(&self, func: &str, base: *const u8) -> Option<extern "C" fn() -> u64> {
        if let Some(f) = self.func_addrs.get(func) {
            use std::mem::transmute;
            Some(transmute(base.add(f.start)))
        } else {
            None
        }
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn disassemble(&self, base: *const u8) {
        use iced_x86::{Decoder, DecoderOptions, Formatter, Instruction, NasmFormatter};

        for (name, range) in self.func_addrs.iter() {
            println!("\n{}({}):", name, unsafe {
                *(self.data.as_ptr().add(range.start - 16) as *const usize)
            });
            let bytes = &self.data[range.start..range.end];
            let mut decoder = Decoder::with_ip(
                64,
                bytes,
                base as u64 + range.start as u64,
                DecoderOptions::NONE,
            );

            let mut formatter = NasmFormatter::new();

            formatter.options_mut().set_digit_separator("`");
            formatter.options_mut().set_first_operand_char_index(0);

            let mut output = String::new();
            let mut instruction = Instruction::default();
            while decoder.can_decode() {
                decoder.decode_out(&mut instruction);

                output.clear();
                formatter.format(&instruction, &mut output);

                print!("{:016X}\n    ", instruction.ip());
                let start_index = instruction.ip() as usize - base as usize;
                let instr_bytes = &self.data[start_index..start_index + instruction.len()];
                for b in instr_bytes.iter() {
                    print!("{:02X}", b);
                }
                if instr_bytes.len() < 10 {
                    for _ in 0..10 - instr_bytes.len() {
                        print!("  ");
                    }
                }
                println!(" {}", output);
            }
        }
    }

    fn generate_mov(&mut self, dest: Register, source: Register) {
        let dest_location = dest.convert_to_instr_arg();
        let source_location = source.convert_to_instr_arg();

        match (dest_location.is_register(), source_location.is_register()) {
            (true, true) => {
                // mov dest_reg, source_reg
                self.data
                    .push(0x48 | dest_location.is_64_bit() | (source_location.is_64_bit() << 2));
                self.data.push(0x89);
                self.data.push(
                    0xc0 | dest_location.get_register() | (source_location.get_register() << 3),
                );
            }

            (true, false) => {
                // mov rax, [rbp +- offset]
                self.data.push(0x48 | (dest_location.is_64_bit() << 2));
                self.data.push(0x8b);
                self.data.push(0x85 | (dest_location.get_register() << 3));

                let offset = if let InstructionRegister::Arg(a) = source_location {
                    (a as u32 + 2) * 8
                } else if let InstructionRegister::Spilled(s) = source_location {
                    (-(s as i32 + 1) * 8) as u32
                } else {
                    unreachable!();
                };

                self.data.push((offset & 0xff) as u8);
                self.data.push(((offset >> 8) & 0xff) as u8);
                self.data.push(((offset >> 16) & 0xff) as u8);
                self.data.push(((offset >> 24) & 0xff) as u8);
            }

            (false, true) => {
                // mov [rbp +- offset], rax
                self.data.push(0x48 | (source_location.is_64_bit() << 2));
                self.data.push(0x89);
                self.data.push(0x85 | (source_location.get_register() << 3));

                let offset = if let InstructionRegister::Arg(a) = dest_location {
                    (a as u32 + 2) * 8
                } else if let InstructionRegister::Spilled(s) = dest_location {
                    (-(s as i32 + 1) * 8) as u32
                } else {
                    unreachable!();
                };

                self.data.push((offset & 0xff) as u8);
                self.data.push(((offset >> 8) & 0xff) as u8);
                self.data.push(((offset >> 16) & 0xff) as u8);
                self.data.push(((offset >> 24) & 0xff) as u8);
            }

            (false, false) => {
                // mov rax, [rbp +- offset]
                self.generate_mov(Register::Rax, source);

                // mov [rbp +- offset], rax
                self.generate_mov(dest, Register::Rax);
            }
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
        code.func_addrs
            .insert(func.name.clone(), code.len()..code.len() + 1);

        // push rbp
        code.data.push(0x55);

        // mov rbp, rsp
        code.generate_mov(Register::Rbp, Register::Rsp);

        common::linear_scan(func, NONARG_REGISTER_COUNT);

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
                        code.generate_mov(Register::Rax, *register);
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
                    code.generate_mov(Register::Rsp, Register::Rbp);

                    // pop rbp
                    code.data.push(0x5d);

                    // ret
                    code.data.push(0xc3);
                }

                IrInstruction::Load => {
                    if let Some(local) = ssa.local {
                        let local_reg = *local_to_register.get(&local).unwrap();
                        let local_location = local_reg.convert_to_instr_arg();

                        match ssa.args.first() {
                            Some(IrArgument::Argument(arg)) => {
                                // mov local, [rbp + offset]
                                code.generate_mov(
                                    local_reg,
                                    Register::convert_arg_register_id(*arg),
                                );
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

                                    if local_location.get_offset() >= stack_allocated_local_count {
                                        stack_allocated_local_count += 1;

                                        // push rax
                                        code.data.push(0x50);
                                    } else {
                                        // mov [rbp - offset], rax
                                        code.generate_mov(local_reg, Register::Rax);
                                    }
                                }
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
                            let arg_location = arg_reg.convert_to_instr_arg();

                            match arg {
                                IrArgument::Local(local) => {
                                    let local_reg = *local_to_register.get(local).unwrap();

                                    // mov arg, local
                                    code.generate_mov(arg_reg, local_reg);
                                }

                                IrArgument::Argument(arg) => {
                                    let local_reg = Register::convert_arg_register_id(*arg);

                                    // mov arg, local
                                    code.generate_mov(arg_reg, local_reg);
                                }

                                IrArgument::Function(func) => {
                                    // mov arg, func
                                    code.data.push(0x48 | arg_location.is_64_bit());
                                    code.data.push(0xb8 | arg_location.get_register());

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
                                        code.generate_mov(Register::Rax, local_reg);

                                        // push rax
                                        code.data.push(0x50);
                                    }
                                }

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
                            code.generate_mov(local_reg, Register::Rax);
                        }
                    }
                }
            }
        }
        code.func_addrs.get_mut(&func.name).unwrap().end = code.len();
    }

    code
}
