use super::ir::{Ir, IrModule, Location, SExpr};
use super::types::Type;

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

        SExpr::Function(_, _) => { }

        SExpr::ExternalFunc(_, _, _) => todo!(),

        SExpr::Chain(_, _, _) => todo!(),

        SExpr::Application(m, f, a) => {
            check_sexpr(f, module, errors);
            check_sexpr(a, module, errors);

            if let Type::Func(arg, ret) = &*f.get_metadata()._type {

            } else {
                panic!("type {} is not a function", f.get_metadata()._type);
            }
        }

        SExpr::Assign(_, _, _) => todo!(),

        SExpr::With(_, _, _) => todo!(),

        SExpr::Match(_, _, _) => todo!(),
    }
}

pub fn check_correctness(ir: &mut Ir, require_main: bool) -> Result<(), Vec<CorrectnessError>> {
    let mut errors = vec![];

    for (_, module) in ir.modules.iter_mut() {
        let keys: Vec<String> = module.funcs.keys().cloned().collect();
        for key in keys {
            module.scope.push_scope(true);

            let mut func = module.funcs.remove(&key).unwrap();
            for a in func.args.iter() {
                module.scope.put_var(&a.0, &a.1, 0, &Location::empty(), true, "");
            }

            check_sexpr(&mut func.body, module, &mut errors);

            module.funcs.insert(key, func);
            module.scope.pop_scope();
        }

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
