use super::ir::IrFunction;

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
