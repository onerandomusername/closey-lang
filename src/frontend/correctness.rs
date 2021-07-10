use logos::Span;
use std::collections::HashMap;
use std::sync::Arc;

use super::ir::{ArityInfo, Ir, IrFunction, IrModule, Location, SExpr, SExprMetadata};
use super::types::{arc, Type};

pub enum CorrectnessError {}

fn check_sexpr(
    parent_func: &mut IrFunction,
    sexpr: &mut SExpr,
    module: &mut IrModule,
    errors: &mut Vec<CorrectnessError>,
) {
    match sexpr {
        SExpr::Empty(_) => todo!(),

        SExpr::TypeAlias(_, _) => todo!(),

        SExpr::Symbol(m, s) => {
            if let Some((_type, arity, _, _, _)) = module.scope.get_var(s) {
                m._type = _type.clone();
                m.arity = *arity;
                if module.scope.is_captured(s) && !parent_func.captured_names.contains(s) {
                    parent_func.captured_names.push(s.clone());
                    parent_func.captured.insert(s.clone(), _type.clone());
                }
            } else if let Some(func) = module.globals.get(s) {
                *sexpr = SExpr::Function(m.clone(), func.clone());
                check_sexpr(parent_func, sexpr, module, errors);
            } else {
                panic!("variable {} not found", s);
            }
        }

        SExpr::Function(m, f) => {
            if let Some(func) = module.funcs.get(f) {
                if func.checked {
                    m._type = func._type.clone();
                    m.arity = ArityInfo::Known(func.args.len());
                } else {
                    let mut func = module.funcs.remove(f).unwrap();
                    module.scope.push_scope(true);

                    for arg in func.args.iter() {
                        module.scope.put_var(
                            &arg.0,
                            &arg.1,
                            ArityInfo::Unknown,
                            &Location::empty(),
                            true,
                            &module.name,
                        );
                    }

                    use std::mem::swap;
                    let mut body = SExpr::Empty(SExprMetadata::empty());
                    swap(&mut func.body, &mut body);
                    check_sexpr(&mut func, &mut body, module, errors);
                    swap(&mut func.body, &mut body);

                    module.scope.pop_scope();

                    let mut _type = func.body.get_metadata()._type.clone();
                    for arg in func.args.iter().rev() {
                        _type = arc::new(Type::Func(arg.1.clone(), _type));
                    }

                    func._type = _type;
                    m._type = func._type.clone();
                    m.arity = ArityInfo::Known(func.args.len());

                    func.checked = true;
                    module.funcs.insert(f.clone(), func);
                }
            } else {
                panic!("this shouldn't happen i believe");
            }
        }

        SExpr::ExternalFunc(_, _, _) => todo!(),

        SExpr::Chain(_, _, _) => todo!(),

        SExpr::Application(m, func, args) => {
            check_sexpr(parent_func, func, module, errors);
            for arg in args.iter_mut() {
                check_sexpr(parent_func, arg, module, errors);
            }

            let mut ft = func.get_metadata()._type.clone();
            let mut generics_map = HashMap::new();

            use std::mem::swap;
            let mut args_temp = vec![];
            swap(&mut args_temp, args);
            let mut arity = func.get_metadata().arity;
            let last_index = args_temp.len();
            for (i, arg) in args_temp.into_iter().enumerate() {
                if let Type::Func(at, rt) = &*ft {
                    if arg
                        .get_metadata()
                        ._type
                        .is_subtype(at, &module.types, &mut generics_map)
                    {
                        m._type = rt.clone();
                        ft = rt.clone();
                        Arc::make_mut(&mut m._type).replace_generics(&generics_map);
                    } else {
                        panic!("{} is not a subtype of {}", arg.get_metadata()._type, at);
                    }

                    args.push(arg);

                    arity = match arity {
                        ArityInfo::Known(v) if v > 0 => ArityInfo::Known(v - 1),
                        ArityInfo::Known(_) => ArityInfo::Unknown,
                        ArityInfo::Unknown => ArityInfo::Unknown,
                    };

                    if i != last_index - 1 && matches!(arity, ArityInfo::Known(0)) {
                        let mut temp = vec![];
                        swap(&mut temp, args);
                        **func = SExpr::Application(
                            SExprMetadata {
                                loc: Location::new(
                                    Span {
                                        start: m.loc.span.start,
                                        end: temp.last().unwrap().get_metadata().loc.span.end,
                                    },
                                    &m.loc.filename,
                                ),
                                loc2: Location::empty(),
                                origin: m.origin.clone(),
                                _type: {
                                    let mut ft = ft.clone();
                                    Arc::make_mut(&mut ft).replace_generics(&generics_map);
                                    ft
                                },
                                arity,
                                tailrec: false,
                                impure: false,
                            },
                            func.clone(),
                            temp,
                        );
                    }
                } else {
                    panic!("type {} is not a function", func.get_metadata()._type);
                }
            }

            m._type = ft;
            m.arity = arity;
            Arc::make_mut(&mut m._type).replace_generics(&generics_map);
        }

        SExpr::Assign(m, a, v) => {
            check_sexpr(parent_func, v, module, errors);
            m._type = v.get_metadata()._type.clone();
            m.arity = v.get_metadata().arity;
            module
                .scope
                .put_var(a, &m._type, m.arity, &m.loc, true, &module.name);
        }

        SExpr::With(_, _, _) => todo!(),

        SExpr::Match(_, _, _) => todo!(),
    }
}

pub fn check_correctness(ir: &mut Ir, _require_main: bool) -> Result<(), Vec<CorrectnessError>> {
    let mut errors = vec![];

    for (_, module) in ir.modules.iter_mut() {
        let globals = module.globals.clone();
        for (_, raw) in globals {
            use std::mem::swap;

            let mut func = module.funcs.remove(&raw).unwrap();
            if func.checked {
                module.funcs.insert(raw, func);
                continue;
            }

            module.scope.push_scope(true);
            for arg in func.args.iter() {
                module.scope.put_var(
                    &arg.0,
                    &arg.1,
                    ArityInfo::Unknown,
                    &Location::empty(),
                    true,
                    "",
                );
            }

            let mut body = SExpr::Empty(SExprMetadata::empty());
            swap(&mut func.body, &mut body);
            check_sexpr(&mut func, &mut body, module, &mut errors);
            swap(&mut func.body, &mut body);

            let mut _type = func.body.get_metadata()._type.clone();
            for arg in func.args.iter().rev() {
                _type = arc::new(Type::Func(arg.1.clone(), _type));
            }

            func._type = _type;

            module.scope.pop_scope();

            module.funcs.insert(raw, func);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
