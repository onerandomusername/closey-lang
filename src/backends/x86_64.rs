pub mod codegen;

use super::GeneratedCode;

/// Disassembles x86 machine code into human readable assembly to stdout.
pub fn disassemble(code: &GeneratedCode, base: *const u8) {
    use iced_x86::{Decoder, DecoderOptions, Formatter, Instruction, NasmFormatter};

    for (name, range) in code.func_addrs.iter() {
        println!("\n{}:", name);
        let bytes = &code.data[range.start..range.end];
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
            let instr_bytes = &code.data[start_index..start_index + instruction.len()];
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

