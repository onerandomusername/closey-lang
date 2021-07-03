use std::collections::HashMap;
use std::fmt::Display;

use super::super::frontend::ir::{self, ArityInfo, SExpr, SExprMetadata};

/// An instruction in the low level intermediate representation.
#[derive(Copy, Clone)]
pub enum IrInstruction {
    /// Returns an optional parameter from a function.
    Ret,

    /// Loads a function or argument parameter into a local.
    Load,

    /// Applies a list of arguments to a function pointer or closure struct to form a new closure
    /// struct.
    Apply,

    /// Calls a function, function pointer, or closure struct and passes the return value into a
    /// new local value. True if the arity is known at compile time, false otherwise.
    Call(bool),
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrInstruction::*;
        match self {
            Ret => write!(f, "ret"),
            Load => write!(f, "load"),
            Apply => write!(f, "apply"),
            Call(known_arity) => write!(f, "call{}", if *known_arity { "" } else { "?" }),
        }
    }
}

/// An argument passed into an instruction in the low level intermediate representation.
pub enum IrArgument {
    /// A local value.
    Local(usize),

    /// An argument passed into the function that contains the instruction. Closed values are also
    /// considered arguments.
    Argument(usize),

    /// A function address.
    Function(String),
}

impl Display for IrArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrArgument::*;
        match self {
            Local(l) => write!(f, "%{}", l),
            Argument(a) => write!(f, "${}", a),
            Function(g) => write!(f, "@{}", g),
        }
    }
}

/// Represents a single instruction in the lower level intermediate representation.
pub struct IrSsa {
    /// The local value the instruction is assigned to.
    pub local: Option<usize>,

    /// The lifetime of the local assigned in this statement.
    pub local_lifetime: usize,

    /// The register the local assigned to in this instruction is allocated in.
    pub local_register: usize,

    /// The instruction (ie opcode) being executed in this instruction.
    pub instr: IrInstruction,

    /// The arguments passed into the instruction.
    pub args: Vec<IrArgument>,
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

/// A function in the lower level intermediate representation.
pub struct IrFunction {
    /// The name of the function.
    pub name: String,

    /// The number of arguments (including closed over values) that the function takes in.
    pub argc: usize,

    /// The list of all SSAs associated with this function.
    /// TODO: Replace with basic blocks.
    pub ssas: Vec<IrSsa>,
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

/// A module in lower level intermediate representation.
/// TODO: Have a higher level data structure that represents the list of all modules in the code.
pub struct IrModule {
    /// The list of all functions in the module.
    pub funcs: Vec<IrFunction>,
}

impl Display for IrModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.funcs.iter() {
            write!(f, "{}\n\n", func)?;
        }
        Ok(())
    }
}

fn get_arg_if_applicable(
    args_map: &HashMap<String, usize>,
    sexpr: SExpr,
    map: &HashMap<String, Vec<String>>,
) -> Result<IrArgument, SExpr> {
    match sexpr {
        SExpr::Symbol(_, s) => {
            if let Some(a) = args_map.get(&s) {
                Ok(IrArgument::Argument(*a))
            } else {
                todo!("symbols that aren't arguments");
            }
        }

        SExpr::Function(_, f) if map.get(&f).unwrap().is_empty() => Ok(IrArgument::Function(f)),

        _ => Err(sexpr),
    }
}

fn conversion_helper(
    args_map: &HashMap<String, usize>,
    func: &mut IrFunction,
    sexpr: SExpr,
    map: &HashMap<String, Vec<String>>,
) -> Option<usize> {
    match get_arg_if_applicable(args_map, sexpr, map) {
        Ok(v) => {
            let local = Some(func.get_next_local());
            func.ssas.push(IrSsa {
                local,
                local_lifetime: 0,
                local_register: 0,
                instr: IrInstruction::Load,
                args: vec![v],
            });
            local
        }

        Err(SExpr::Empty(_)) => todo!(),
        Err(SExpr::TypeAlias(_, _)) => todo!(),

        Err(SExpr::ExternalFunc(_, _, _)) => todo!(),
        Err(SExpr::Chain(_, _, _)) => todo!(),

        Err(SExpr::Function(_, f)) => {
            use std::iter::once;
            let local = Some(func.get_next_local());
            let args = map.get(&f).unwrap().iter().map(|v| get_arg_if_applicable(args_map, SExpr::Symbol(SExprMetadata::empty(), v.clone()), map).unwrap());
            func.ssas.push(IrSsa {
                local,
                local_lifetime: 0,
                local_register: 0,
                instr: IrInstruction::Apply,
                args: once(IrArgument::Function(f)).chain(args).collect()
            });
            local
        }

        Err(SExpr::Application(m, f, a)) => {
            let f = match get_arg_if_applicable(args_map, *f, map) {
                Ok(v) => v,
                Err(e) => IrArgument::Local(conversion_helper(args_map, func, e, map).unwrap()),
            };

            let args: Vec<_> = a
                .into_iter()
                .map(|a| match get_arg_if_applicable(args_map, a, map) {
                    Ok(v) => v,
                    Err(e) => IrArgument::Local(conversion_helper(args_map, func, e, map).unwrap()),
                })
                .collect();

            use std::iter::once;
            let local = Some(func.get_next_local());
            if matches!(m.arity, ArityInfo::Known(v) if v != 0) {
                func.ssas.push(IrSsa {
                    local,
                    local_lifetime: 0,
                    local_register: 0,
                    instr: IrInstruction::Apply,
                    args: once(f).chain(args.into_iter()).collect(),
                });
            } else {
                func.ssas.push(IrSsa {
                    local,
                    local_lifetime: 0,
                    local_register: 0,
                    instr: IrInstruction::Call(matches!(m.arity, ArityInfo::Known(_))),
                    args: once(f).chain(args.into_iter()).collect(),
                });
            }

            local
        }

        Err(SExpr::Assign(_, _, _)) => todo!(),
        Err(SExpr::With(_, _, _)) => todo!(),
        Err(SExpr::Match(_, _, _)) => todo!(),

        Err(SExpr::Symbol(_, _)) => unreachable!(),
    }
}

fn calculate_lifetimes(func: &mut IrFunction) {
    let mut iter = func.ssas.iter_mut();
    let mut i = 0;
    while let Some(ssa) = iter.next() {
        if ssa.local.is_none() {
            continue;
        }
        let local = ssa.local.unwrap();

        let mut j = i + 1;
        for next in iter.as_slice() {
            for arg in next.args.iter() {
                if let IrArgument::Local(l) = arg {
                    if *l == local {
                        ssa.local_lifetime = j - i;
                        break;
                    }
                }
            }

            j += 1;
        }

        i += 1;
    }
}

/// Converts the frontend IR language to the backend IR language.
pub fn convert_frontend_ir_to_backend_ir(module: ir::IrModule) -> IrModule {
    let mut new = IrModule { funcs: vec![] };

    let map: HashMap<_, _> = module.funcs.iter().map(|v| (v.0.clone(), v.1.captured_names.clone())).collect();
    for func in module.funcs {
        let mut f = IrFunction {
            name: func.1.name,
            argc: func.1.args.len() + func.1.captured.len(),
            ssas: vec![],
        };
        let args_map: HashMap<String, usize> = func
            .1
            .captured_names
            .into_iter()
            .enumerate()
            .chain(func.1.args.into_iter().map(|v| v.0).enumerate())
            .map(|v| (v.1, v.0))
            .collect();

        conversion_helper(&args_map, &mut f, func.1.body, &map);
        f.ssas.push(IrSsa {
            local: None,
            local_lifetime: 0,
            local_register: 0,
            instr: IrInstruction::Ret,
            args: if let Some(l) = f.get_last_local() {
                vec![IrArgument::Local(l)]
            } else {
                vec![]
            },
        });

        calculate_lifetimes(&mut f);

        new.funcs.push(f);
    }

    new
}
