use logos::Span;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::ir::Location;
use super::parser::Ast;

#[derive(Clone, Debug)]
pub struct HashSetWrapper<T>(pub HashSet<T>);

impl<T: Hash + Eq> PartialEq for HashSetWrapper<T> {
    fn eq(&self, other: &HashSetWrapper<T>) -> bool {
        self.0 == other.0
    }
}

impl<T: Hash + Eq> Eq for HashSetWrapper<T> {}

impl<T: Hash + Eq> Hash for HashSetWrapper<T> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        let mut hash: u64 = 0;
        for v in self.0.iter() {
            let mut h_ = DefaultHasher::new();
            v.hash(&mut h_);
            hash ^= h_.finish();
        }
        hash.hash(h);
    }
}

pub type TypeRc = Arc<Type>;

// Represents a type.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Error,
    UndeclaredTypeError(Location),
    DuplicateTypeError(Location, Location, TypeRc),
    Unknown,
    Int,
    Float,
    Bool,
    Word,
    Char,
    Symbol(String),
    Generic(String),
    Func(TypeRc, TypeRc),
    Curried(TypeRc, TypeRc),
    Union(HashSetWrapper<TypeRc>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            // Errors
            Type::Error => {
                write!(f, "{{ unknown }}")?;
            }
            Type::UndeclaredTypeError(_) => {
                write!(f, "UndeclaredTypeError")?;
            }
            Type::DuplicateTypeError(_, _, _) => {
                write!(f, "DuplicateTypeError")?;
            }
            Type::Unknown => {
                write!(f, "{{ unknown }}")?;
            }

            // Primitives
            Type::Int => {
                write!(f, "Int")?;
            }
            Type::Float => {
                write!(f, "Float")?;
            }
            Type::Bool => {
                write!(f, "Bool")?;
            }
            Type::Word => {
                write!(f, "Word")?;
            }
            Type::Char => {
                write!(f, "Char")?;
            }
            Type::Symbol(s) => {
                write!(f, "{}", s)?;
            }
            Type::Generic(g) => {
                write!(f, "'{}", g)?;
            }

            // Function types
            Type::Func(arg, ret) => {
                if let Type::Func(_, _) = **arg {
                    write!(f, "({})", **arg)?;
                } else {
                    write!(f, "{}", **arg)?;
                }
                write!(f, " -> {}", ret)?;
            }

            // Curried types
            Type::Curried(arg, ret) => {
                if let Type::Func(_, _) = **arg {
                    write!(f, "({})", **arg)?;
                } else if let Type::Curried(_, _) = **arg {
                    write!(f, "({})", **arg)?;
                } else {
                    write!(f, "{}", **arg)?;
                }
                write!(f, " +> {}", ret)?;
            }

            // Union types
            Type::Union(fields) => {
                let mut bar = false;
                for field in fields.0.iter() {
                    if bar {
                        write!(f, " | ")?;
                    } else {
                        bar = true;
                    }

                    if let Type::Func(_, _) = **field {
                        write!(f, "({})", field)?;
                    } else {
                        write!(f, "{}", field)?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl Type {
    // sum_hash(&self) -> u64
    // Returns the hash value used by codegenned sum/union types.
    pub fn sum_hash(&self) -> u64 {
        let mut hash = DefaultHasher::new();
        self.hash(&mut hash);
        hash.finish()
    }

    // is_subtype(&self, &Type, &HashMap<String, Type>) -> bool
    // Returns true if self is a valid subtype in respect to the passed in type.
    pub fn is_subtype(&self, supertype: &Type, types: &HashMap<String, TypeRc>, generics_map: &mut HashMap<String, TypeRc>) -> bool {
        let _type = self;

        if _type == supertype {
            return true;
        }

        match supertype {
            // Primitives
            Type::Int => *_type == Type::Int,
            Type::Float => *_type == Type::Float,
            Type::Bool => *_type == Type::Bool,
            Type::Word => *_type == Type::Word,
            Type::Char => *_type == Type::Char,

            // Functions
            Type::Func(sf, sa) => {
                if let Type::Func(f, a) = _type {
                    f.is_subtype(sf, types, generics_map) && a.is_subtype(sa, types, generics_map)
                } else {
                    false
                }
            }

            // Curried
            Type::Curried(sf, sa) => {
                if let Type::Curried(f, a) = _type {
                    f.is_subtype(sf, types, generics_map) && a.is_subtype(sa, types, generics_map)
                } else {
                    false
                }
            }

            // Generics
            Type::Generic(g) => {
                if let Some(t) = generics_map.get(g) {
                    &**t == supertype
                } else {
                    generics_map.insert(g.clone(), arc::new(self.clone()));
                    true
                }
            }

            // Union types
            Type::Union(fields) => {
                // Union types mean the subtype has fields over a subset of fields of the supertype
                if let Type::Union(sub) = _type {
                    for s in sub.0.iter() {
                        let mut is_subtype = false;
                        for f in fields.0.iter() {
                            if s.is_subtype(&f, types, generics_map) {
                                is_subtype = true;
                                break;
                            }
                        }

                        if !is_subtype {
                            return false;
                        }
                    }

                    return true;
                }

                for t in fields.0.iter() {
                    if _type.is_subtype(t, types, generics_map) {
                        return true;
                    }
                }

                false
            }

            // Everything else is to be ignored
            Type::Error
            | Type::UndeclaredTypeError(_)
            | Type::DuplicateTypeError(_, _, _)
            | Type::Unknown
            | Type::Symbol(_) => false,
        }
    }

    pub fn replace_generics(&mut self, generics_map: &HashMap<String, TypeRc>) {
        match self {
            // Functions
            Type::Func(f, a) => {
                Arc::make_mut(f).replace_generics(generics_map);
                Arc::make_mut(a).replace_generics(generics_map);
            }

            // Curried
            Type::Curried(f, a) => {
                Arc::make_mut(f).replace_generics(generics_map);
                Arc::make_mut(a).replace_generics(generics_map);
            }

            // Generics
            Type::Generic(g) => {
                if let Some(t) = generics_map.get(g) {
                    *self = (**t).clone();
                }
            }

            // Union types
            Type::Union(_fields) => {
                todo!();
            }

            // Everything else is to be ignored
            Type::Error
            | Type::UndeclaredTypeError(_)
            | Type::DuplicateTypeError(_, _, _)
            | Type::Unknown
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::Word
            | Type::Char
            | Type::Symbol(_) => { }
        }
    }
}

// ast_sum_builder_helper(Ast, &str, &mut HashMap<TypeRc, Span>) -> Type
// Helper function for building sum/union types.
fn ast_sum_builder_helper(ast: Ast, filename: &str, fields: &mut HashMap<TypeRc, Span>) -> Type {
    let s = ast.get_span();
    let v = convert_ast_to_type(ast, filename);
    if let Type::Union(v) = v {
        for v in v.0 {
            if let Some(s2) = fields.remove(&v) {
                return Type::DuplicateTypeError(
                    Location::new(s, filename),
                    Location::new(s2, filename),
                    v,
                );
            } else {
                fields.insert(v, s.clone());
            }
        }
    } else {
        let v = arc::new(v);
        if let Some(s2) = fields.remove(&v) {
            return Type::DuplicateTypeError(
                Location::new(s, filename),
                Location::new(s2, filename),
                v,
            );
        } else {
            fields.insert(v, s);
        }
    }

    Type::Unknown
}

// convert_ast_to_type(Ast, &IR) -> Type
// Converts an ast node into a type.
pub fn convert_ast_to_type(ast: Ast, filename: &str) -> Type {
    match ast {
        // Symbols
        Ast::Symbol(_, v) => {
            match v.as_str() {
                // Primitives
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Bool" => Type::Bool,
                "Word" => Type::Word,
                "Char" => Type::Char,

                // Symbol
                _ => Type::Symbol(v),
            }
        }

        // Generics
        Ast::Generic(_, g) => {
            Type::Generic(g)
        }

        // Sum types
        Ast::Infix(_, op, l, r) if op == "|" => {
            let mut fields = HashMap::new();
            let mut acc = *l;
            let t = ast_sum_builder_helper(*r, filename, &mut fields);
            if t != Type::Unknown {
                return t;
            }

            loop {
                match acc {
                    Ast::Infix(_, op, l, r) if op == "|" => {
                        let t = ast_sum_builder_helper(*r, filename, &mut fields);
                        if t != Type::Unknown {
                            return t;
                        }

                        acc = *l;
                    }

                    _ => break,
                }
            }

            let t = ast_sum_builder_helper(acc, filename, &mut fields);
            if t != Type::Unknown {
                return t;
            }

            for f in fields.iter() {
                if let Type::UndeclaredTypeError(s) = &**f.0 {
                    return Type::UndeclaredTypeError(s.clone());
                }
            }

            if fields.len() == 1 {
                (*fields.into_iter().next().unwrap().0).clone()
            } else {
                Type::Union(HashSetWrapper(
                    fields.into_iter().map(|v| v.0).collect()
                ))
            }
        }

        // Curried
        Ast::Infix(_, op, l, r) if op == "+>" => {
            let l = convert_ast_to_type(*l, filename);
            let r = convert_ast_to_type(*r, filename);

            if let Type::UndeclaredTypeError(s) = l {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = l {
                Type::DuplicateTypeError(a, b, c)
            } else if let Type::UndeclaredTypeError(s) = r {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = r {
                Type::DuplicateTypeError(a, b, c)
            } else {
                Type::Curried(arc::new(l), arc::new(r))
            }
        }

        // Function types
        Ast::Infix(_, op, l, r) if op == "->" => {
            let l = convert_ast_to_type(*l, filename);
            let r = convert_ast_to_type(*r, filename);

            if let Type::UndeclaredTypeError(s) = l {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = l {
                Type::DuplicateTypeError(a, b, c)
            } else if let Type::UndeclaredTypeError(s) = r {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = r {
                Type::DuplicateTypeError(a, b, c)
            } else {
                Type::Func(arc::new(l), arc::new(r))
            }
        }

        // Error
        _ => Type::UndeclaredTypeError(Location::new(ast.get_span(), filename)),
    }
}

pub mod arc {
    use std::sync::Arc;
    use super::Type;

    pub fn new(t: Type) -> Arc<Type> {
        Arc::new(t)
    }
}

