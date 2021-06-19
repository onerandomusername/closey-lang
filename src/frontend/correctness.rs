use std::collections::HashMap;
use std::sync::Arc;

use super::ir::{Ir, IrModule, Location, SExpr};
use super::types::{arc, Type};

pub enum CorrectnessError {
}

fn check_sexpr(sexpr: &mut SExpr, module: &mut IrModule, errors: &mut Vec<CorrectnessError>) {
    match sexpr {
        SExpr::Empty(_) => todo!(),

        SExpr::TypeAlias(_, _) => todo!(),

        SExpr::Symbol(m, s) => {
            if let Some((_type, arity, _, _, _)) = module.scope.get_var(s) {
                m._type = _type.clone();
                m.arity = *arity;
            } else {
                panic!("variable {} not found", s);
            }
        }

        SExpr::Function(m, f) => {
            if let Some(func) = module.funcs.get(f) {
                if func.checked {
                    m._type = func._type.clone();
                    m.arity = func.args.len();
                } else {
                    let mut func = module.funcs.remove(f).unwrap();
                    module.scope.push_scope(true);

                    for arg in func.args.iter() {
                        module.scope.put_var(&arg.0, &arg.1, 0, &Location::empty(), true, &module.name);
                    }

                    check_sexpr(&mut func.body, module, errors);

                    module.scope.pop_scope();

                    let mut _type = func.body.get_metadata()._type.clone();
                    for arg in func.args.iter().rev() {
                        _type = arc::new(Type::Func(arg.1.clone(), _type));
                    }
                    func._type = _type;
                    m._type = func._type.clone();
                    m.arity = func.args.len();

                    func.checked = true;
                    module.funcs.insert(f.clone(), func);
                }
            } else {
                panic!("this shouldn't happen i believe");
            }
        }

        SExpr::ExternalFunc(_, _, _) => todo!(),

        SExpr::Chain(_, _, _) => todo!(),

        SExpr::Application(m, f, a) => {
            check_sexpr(f, module, errors);
            check_sexpr(a, module, errors);

            let ft = &*f.get_metadata()._type;
            let ft = if let Type::Curried(_, f) = ft { f } else { ft };
            if let Type::Func(arg, ret) = ft {
                let mut generics_map = HashMap::new();
                if a.get_metadata()._type.is_subtype(arg, &module.types, &mut generics_map) {
                    m._type = ret.clone();
                    Arc::make_mut(&mut m._type).replace_generics(&generics_map);

                    if f.get_metadata().arity != 0 {
                        m.arity = f.get_metadata().arity - 1;
                    }
                } else {
                    panic!("{} is not a subtype of {}", a, arg);
                }
            } else {
                panic!("type {} is not a function", f.get_metadata()._type);
            }
        }

        SExpr::Assign(m, a, v) => {
            check_sexpr(v, module, errors);
            m._type = v.get_metadata()._type.clone();
            module.scope.put_var(a, &m._type, m.arity, &m.loc, true, &module.name);
        }

        SExpr::With(_, _, _) => todo!(),

        SExpr::Match(_, _, _) => todo!(),
    }
}

pub fn check_correctness(ir: &mut Ir, _require_main: bool) -> Result<(), Vec<CorrectnessError>> {
    let mut errors = vec![];

    for (_, module) in ir.modules.iter_mut() {
        use std::mem::swap;
        let mut v = vec![];
        swap(&mut v, &mut module.sexprs);

        for sexpr in v.iter_mut() {
            check_sexpr(sexpr, module, &mut errors);
        }

        swap(&mut v, &mut module.sexprs);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
