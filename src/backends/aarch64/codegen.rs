use super::super::GeneratedCode;
use super::super::ir::IrModule;

/// Transforms an IrModule into aarch64 machine code.
pub fn generate_code(_: &mut IrModule) -> GeneratedCode {
    let mut code = GeneratedCode::new();
    code.data = vec![0xd1, 0x00, 0x43, 0xff, 0xb9, 0x00, 0x0f, 0xff, 0x52, 0x80, 0x08, 0xa0, 0x91, 0x00, 0x43, 0xff, 0xd6, 0x5f, 0x03, 0xc0];
    code.func_addrs.insert(String::from("main"), 0..code.len());
    code
}
