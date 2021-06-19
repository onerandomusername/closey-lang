use std::collections::HashMap;
use std::fmt::Display;

use super::super::frontend::ir::{self, SExpr};

#[derive(Copy, Clone)]
pub enum IrInstruction {
    Ret,
    Load,
    Func,
    Call,
    Apply
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrInstruction::*;
        match self {
            Ret  => write!(f, "ret" ),
            Load => write!(f, "load"),
            Func => write!(f, "func"),
            Call => write!(f, "call"),
            Apply => write!(f, "apply")
        }
    }
}

#[derive(Clone)]
pub enum IrArgument {
    Local(usize),
    Argument(usize),
    Function(String)
}

impl Display for IrArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrArgument::*;
        match self {
            Local(l) => write!(f, "%{}", l),
            Argument(a) => write!(f, "${}", a),
            Function(g) => write!(f, "{}", g)
        }
    }
}

pub struct IrSsa {
    pub local: Option<usize>,
    pub instr: IrInstruction,
    pub args: Vec<IrArgument>
}

impl Display for IrSsa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(l) = self.local {
            write!(f, "%{} = ", l)?;
        }

        write!(f, "{}", self.instr)?;
        for a in self.args.iter() {
            write!(f, " {}", a)?;
        }
        Ok(())
    }
}

pub struct IrFunction {
    pub name: String,
    pub argc: usize,
    pub ssas: Vec<IrSsa>
}

impl Display for IrFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}):", self.name, self.argc)?;
        for ssa in self.ssas.iter() {
            write!(f, "\n    {}", ssa)?;
        }
        Ok(())
    }
}

impl IrFunction {
    fn get_last_local(&self) -> Option<usize> {
        for ssa in self.ssas.iter().rev() {
            if let Some(l) = ssa.local {
                return Some(l);
            }
        }
        None
    }

    fn get_next_local(&self) -> usize {
        for ssa in self.ssas.iter().rev() {
            if let Some(l) = ssa.local {
                return l + 1;
            }
        }
        0
    }
}

pub struct IrModule {
    pub funcs: Vec<IrFunction>
}

impl Display for IrModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.funcs.iter() {
            write!(f, "{}\n\n", func)?;
        }
        Ok(())
    }
}

fn conversion_helper(args_map: &HashMap<String, usize>, func: &mut IrFunction, sexpr: SExpr) -> Option<usize> {
    match sexpr {
        SExpr::Empty(_) => todo!(),
        SExpr::TypeAlias(_, _) => todo!(),

        SExpr::Symbol(_, s) => {
            if let Some(a) = args_map.get(&s) {
                let local = Some(func.get_next_local());
                func.ssas.push(IrSsa {
                    local,
                    instr: IrInstruction::Load,
                    args: vec![IrArgument::Argument(*a)]
                });
                local
            } else {
                todo!("symbols that aren't arguments");
            }
        }

        SExpr::Function(_, f) => {
            let local = Some(func.get_next_local());
            func.ssas.push(IrSsa {
                local,
                instr: IrInstruction::Func,
                args: vec![IrArgument::Function(f)]
            });
            local
        }

        SExpr::ExternalFunc(_, _, _) => todo!(),
        SExpr::Chain(_, _, _) => todo!(),

        SExpr::Application(m, f, a) => {
            let f = conversion_helper(args_map, func, *f);
            let a = conversion_helper(args_map, func, *a);
            let mut local = f;
            if let Some(f) = local {
                if let Some(a) = a {
                    local = Some(func.get_next_local());
                    func.ssas.push(IrSsa {
                        local,
                        instr: IrInstruction::Apply,
                        args: vec![IrArgument::Local(f), IrArgument::Local(a)]
                    });
                }
            }

            if m.arity == 0 && local.is_some() {
                let last = local.unwrap();
                local = Some(func.get_next_local());
                func.ssas.push(IrSsa {
                    local,
                    instr: IrInstruction::Call,
                    args: vec![IrArgument::Local(last)]
                });
            }
            local
        }

        SExpr::Assign(_, _, _) => todo!(),
        SExpr::With(_, _, _) => todo!(),
        SExpr::Match(_, _, _) => todo!(),
    }
}

// convert_frontend_ir_to_backend_ir(ir::IrModule) -> IrModule
// Converts the frontend IR language to the backend IR language.
pub fn convert_frontend_ir_to_backend_ir(module: ir::IrModule) -> IrModule {
    let mut new = IrModule { funcs: vec![] };

    for func in module.funcs {
        let mut f = IrFunction {
            name: func.1.name,
            argc: func.1.args.len() + func.1.captured.len(),
            ssas: vec![]
        };
        let args_map: HashMap<String, usize> = func.1.captured_names.into_iter().enumerate().chain(func.1.args.into_iter().map(|v| v.0).enumerate()).map(|v| (v.1, v.0)).collect();

        conversion_helper(&args_map, &mut f, func.1.body);
        f.ssas.push(IrSsa {
            local: None,
            instr: IrInstruction::Ret,
            args: if let Some(l) = f.get_last_local() {
                vec![IrArgument::Local(l)]
            } else {
                vec![]
            }
        });

        new.funcs.push(f);
    }

    new
}
