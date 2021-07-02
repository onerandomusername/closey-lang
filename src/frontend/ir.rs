use logos::Span;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;

use super::parser::Ast;
use super::scopes::Scope;
use super::types;
use super::types::{arc, Type, TypeRc};

// Represents a location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub span: Span,
    pub filename: String,
}

impl Location {
    // new(Span, &str) -> Location
    // Creates a new Location.
    pub fn new(span: Span, filename: &str) -> Location {
        Location {
            span,
            filename: String::from(filename),
        }
    }

    // empty() -> Location
    // Creates an empty Location.
    pub fn empty() -> Location {
        Location {
            span: Span { start: 0, end: 0 },
            filename: String::with_capacity(0),
        }
    }
}

// Represents an error in Ir
pub enum IrError {
    InvalidType(Location),
    DuplicateTypeInUnion(Location, Location, TypeRc),
    DoubleExport(Location, Location, String),
    RedefineImportAlias(Location, Location, String),
    UnsupportedAnnotation(Location, String),
    InvalidFFIType(Location, TypeRc),
    DuplicateModule(String, DuplicateModuleInfo),
}

pub enum DuplicateModuleInfo {
    NoSuperset,
    NewSupersetOld,
    OldSupersetNew,
    BothSuperset,
}

// TODO: make arity checker smarter (ie, check for arity dependent on arguments or closed over
// values so something like (\x . x) (\y . y) doesn't yield an unknown arity.)
#[derive(Debug, Clone, Copy)]
pub enum ArityInfo {
    Unknown,
    Known(usize),
}

// Represents metadata associated with sexpressions.
#[derive(Debug, Clone)]
pub struct SExprMetadata {
    pub loc: Location,
    pub loc2: Location,
    pub origin: String,
    pub _type: TypeRc,
    pub arity: ArityInfo,
    pub tailrec: bool,
    pub impure: bool,
}

impl SExprMetadata {
    // empty() -> SExprMetadata
    // Creates empty metadata.
    pub fn empty() -> SExprMetadata {
        SExprMetadata {
            loc: Location::empty(),
            loc2: Location::empty(),
            origin: String::with_capacity(0),
            _type: arc::new(Type::Error),
            arity: ArityInfo::Unknown,
            tailrec: false,
            impure: false,
        }
    }
}

// Represents an s expression
#[derive(Debug, Clone)]
pub enum SExpr {
    // Empty
    Empty(SExprMetadata),

    // Type alias
    TypeAlias(SExprMetadata, String),

    /*
    // Ints
    Int(SExprMetadata, i64),

    // Floats
    Float(SExprMetadata, f64),

    // Words
    Word(SExprMetadata, u64),

    // Chars
    Char(SExprMetadata, u8),
    */
    // Symbols
    Symbol(SExprMetadata, String),

    /*
    // Strings
    String(SExprMetadata, String),

    // Lists
    List(SExprMetadata, Vec<SExpr>),
    */
    // Functions
    Function(SExprMetadata, String),

    // External function application
    ExternalFunc(SExprMetadata, String, Vec<SExpr>),

    // Chain operator
    Chain(SExprMetadata, Box<SExpr>, Box<SExpr>),

    // Function application
    Application(SExprMetadata, Box<SExpr>, Vec<SExpr>),

    // Assignment
    Assign(SExprMetadata, String, Box<SExpr>),

    // Scoping
    With(SExprMetadata, Vec<SExpr>, Box<SExpr>),
    //Walrus(SExprMetadata, String, Box<SExpr>),

    // Match expressions
    Match(SExprMetadata, Box<SExpr>, Vec<(TypeRc, SExpr, Location)>),
    // Member access
    // MemberAccess(SExprMetadata, Vec<String>),
}

impl Display for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Empty(_) => todo!(),
            SExpr::TypeAlias(_, _) => todo!(),
            SExpr::Symbol(m, s) => write!(f, "{}: {}", s, m._type),
            SExpr::Function(m, func) => write!(f, "func-get {}: {}", func, m._type),
            SExpr::ExternalFunc(_, _, _) => todo!(),
            SExpr::Chain(_, _, _) => todo!(),
            SExpr::Application(m, func, args) => {
                write!(f, "({})", func)?;
                for arg in args {
                    if let SExpr::Application(_, _, _) = **func {
                        write!(f, " ({})", arg)?;
                    } else {
                        write!(f, " ({})", arg)?;
                    }
                }
                write!(f, " : {}", m._type)
            }

            SExpr::Assign(m, v, a) => write!(f, "set {}: {} = ({})", v, m._type, a),
            SExpr::With(_, _, _) => todo!(),
            SExpr::Match(_, _, _) => todo!(),
        }
    }
}

impl SExpr {
    // get_metadata(&SExpr) -> &SExprMetadata
    // Returns an immutable reference to the metadata.
    pub fn get_metadata(&self) -> &SExprMetadata {
        match self {
            Self::Empty(m)
            | Self::TypeAlias(m, _)
            /*| Self::Int(m, _)
            | Self::Float(m, _)
            | Self::Word(m, _)
            | Self::Char(m, _)*/
            | Self::Symbol(m, _)
            //| Self::String(m, _)
            //| Self::List(m, _)
            | Self::Function(m, _)
            | Self::ExternalFunc(m, _, _)
            | Self::Chain(m, _, _)
            //| Self::As(m, _)
            | Self::Application(m, _, _)
            | Self::Assign(m, _, _)
            | Self::With(m, _, _)
            //| Self::Walrus(m, _, _)
            | Self::Match(m, _, _) => m
            //| Self::MemberAccess(m, _) => m,
        }
    }

    // get_mutable_metadata(&mut SExpr) -> &mut SExprMetadata
    // Returns a mutable reference to the metadata.
    pub fn get_mutable_metadata(&mut self) -> &mut SExprMetadata {
        match self {
            Self::Empty(m)
            | Self::TypeAlias(m, _)
            /*| Self::Int(m, _)
            | Self::Float(m, _)
            | Self::Word(m, _)
            | Self::Char(m, _)*/
            | Self::Symbol(m, _)
            //| Self::String(m, _)
            //| Self::List(m, _)
            | Self::Function(m, _)
            | Self::ExternalFunc(m, _, _)
            | Self::Chain(m, _, _)
            //| Self::As(m, _)
            | Self::Application(m, _, _)
            | Self::Assign(m, _, _)
            | Self::With(m, _, _)
            //| Self::Walrus(m, _, _)
            | Self::Match(m, _, _) => m
            //| Self::MemberAccess(m, _) => m,
        }
    }
}

// Represents a function in the Ir.
#[derive(Debug)]
pub struct IrFunction {
    pub loc: Location,
    pub name: String,
    pub _type: TypeRc,
    pub args: Vec<(String, TypeRc)>,
    pub captured: HashMap<String, TypeRc>,
    pub captured_names: Vec<String>,
    pub body: SExpr,
    pub global: bool,
    pub checked: bool,
    pub written: bool,
    pub impure: bool,
}

impl Display for IrFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(func {} ", self.name)?;
        for (i, (a, t)) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", a, t)?;
        }
        write!(f, " (\n            {}))", self.body)
    }
}

#[derive(Debug)]
pub struct IrImport {
    pub name: String,
    pub loc: Location,
    pub qualified: bool,
    pub imports: HashMap<String, (TypeRc, usize, bool)>,
}

#[derive(Debug)]
pub struct IrExtern {
    pub loc: Location,
    pub extern_name: String,
    pub arg_types: Vec<TypeRc>,
    pub ret_type: TypeRc,
    pub impure: bool,
}

// Represents a module of the ir.
#[derive(Debug)]
pub struct IrModule {
    pub name: String,
    pub filename: String,
    pub contents: String,
    pub lib: bool,
    pub imports: HashMap<String, IrImport>,
    pub exports: HashMap<String, (Location, TypeRc)>,
    pub externals: HashMap<String, IrExtern>,
    pub scope: Scope,
    pub funcs: HashMap<String, IrFunction>,
    pub types: HashMap<String, TypeRc>,
    pub globals: HashMap<String, String>,
}

impl Display for IrModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(module {}", self.name)?;
        for func in self.funcs.iter() {
            write!(f, "\n        {}", func.1)?;
        }

        for (global, raw) in self.globals.iter() {
            write!(f, "\n        (global {} = func-get {} : {})", global, raw, self.funcs.get(raw).unwrap()._type)?;
        }

        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct Ir {
    pub modules: HashMap<String, IrModule>,
}

impl Display for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for module in self.modules.iter() {
            write!(f, "\n    {}", module.1)?;
        }
        write!(f, ")")
    }
}

impl Default for Ir {
    fn default() -> Ir {
        Ir {
            modules: HashMap::new(),
        }
    }
}

impl IrModule {
    // new() -> IrModule
    // Creates a new IrModule.
    pub fn new(filename: &str, contents: &str) -> IrModule {
        IrModule {
            name: String::with_capacity(0),
            filename: String::from(filename),
            contents: String::from(contents),
            lib: false,
            imports: HashMap::with_capacity(0),
            exports: HashMap::with_capacity(0),
            externals: HashMap::with_capacity(0),

            scope: Scope::new(),
            funcs: HashMap::with_capacity(0),
            types: HashMap::with_capacity(0),
            globals: HashMap::with_capacity(0),
        }
    }
}

// convert_node(Ast, &str, bool, &mut HashMap<String, IrFunction>, &mut HashMap<String, TypeRc>) -> SExpr
// Converts an ast node into an sexpression.
fn convert_node(
    ast: Ast,
    filename: &str,
    funcs: &mut HashMap<String, IrFunction>,
    global: bool,
    seen_funcs: &mut HashMap<String, usize>,
    types: &mut HashMap<String, TypeRc>,
) -> SExpr {
    match ast {
        Ast::Empty => unreachable!("never empty"),

        /*
        // Int
        Ast::Int(span, n) => SExpr::Int(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Int),
                arity: 0,
                tailrec: false,
                impure: false,
            },
            n,
        ),

        // Float
        Ast::Float(span, n) => SExpr::Float(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Float),
                arity: 0,
                tailrec: false,
                impure: false,
            },
            n,
        ),

        // Word
        Ast::Word(span, n) => SExpr::Word(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Word),
                arity: 0,
                tailrec: false,
                impure: false,
            },
            n,
        ),

        // Char
        Ast::Char(span, c) => SExpr::Char(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Char),
                arity: 0,
                tailrec: false,
                impure: false,
            },
            c,
        ),

        Ast::List(span, list) => SExpr::List(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                tailrec: false,
                impure: false,
            },
            list.into_iter()
                .map(|v| convert_node(v, filename, funcs, global, seen_funcs, types))
                .collect(),
        ),
        */
        // Symbol
        Ast::Symbol(span, s) => SExpr::Symbol(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: ArityInfo::Unknown,
                tailrec: false,
                impure: false,
            },
            s,
        ),

        Ast::Generic(_, _)
        | Ast::Annotation(_, _)
        | Ast::Import(_, _, _)
        | Ast::QualifiedImport(_, _, _)
        | Ast::Header(_, _, _, _)
        | Ast::LibHeader(_, _, _)
        | Ast::Extern(_, _, _, _) => {
            unreachable!(
                "generics, annotations, imports, headers, and external declarations are already handled!"
            );
        }

        // String
        /*
            Ast::String(span, s) => {
                let loc = Location::new(span, filename);
                let mut cons = SExpr::Application(
                    SExprMetadata {
                        loc: loc.clone(),
                        loc2: Location::empty(),
                        _type: arc::new(Type::Error),
                        origin: String::with_capacity(0),
                        arity: 0,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(SExpr::Application(
                        SExprMetadata {
                            loc: loc.clone(),
                            loc2: Location::empty(),
                            _type: arc::new(Type::Error),
                            origin: String::with_capacity(0),
                            arity: 0,
                            tailrec: false,
                            impure: false,
                        },
                        Box::new(SExpr::Symbol(
                            SExprMetadata {
                                loc: loc.clone(),
                                loc2: Location::empty(),
                                _type: arc::new(Type::Error),
                                origin: String::with_capacity(0),
                                arity: 0,
                                tailrec: false,
                                impure: false,
                            },
                            String::from("cons_S"),
                        )),
                        Box::new(SExpr::Char(
                            SExprMetadata {
                                loc: loc.clone(),
                                loc2: Location::empty(),
                                _type: arc::new(Type::Char),
                                origin: String::with_capacity(0),
                                arity: 0,
                                tailrec: false,
                                impure: false,
                            },
                            if s.is_empty() {
                                0
                            } else {
                                s.bytes().last().unwrap()
                            },
                        )),
                    )),
                    Box::new(SExpr::Char(
                        SExprMetadata {
                            loc: loc.clone(),
                            loc2: Location::empty(),
                            _type: arc::new(Type::Char),
                            origin: String::with_capacity(0),
                            arity: 0,
                            tailrec: false,
                            impure: false,
                        },
                        0,
                    )),
                );
                if s.is_empty() {
                    cons
                } else {
                    for c in s.bytes().rev().skip(1) {
                        cons = SExpr::Application(
                            SExprMetadata {
                                loc: loc.clone(),
                                loc2: Location::empty(),
                                _type: arc::new(Type::Error),
                                origin: String::with_capacity(0),
                                arity: 0,
                                tailrec: false,
                                impure: false,
                            },
                            Box::new(SExpr::Application(
                                SExprMetadata {
                                    loc: loc.clone(),
                                    loc2: Location::empty(),
                                    _type: arc::new(Type::Error),
                                    origin: String::with_capacity(0),
                                    arity: 0,
                                    tailrec: false,
                                    impure: false,
                                },
                                Box::new(SExpr::Symbol(
                                    SExprMetadata {
                                        loc: loc.clone(),
                                        loc2: Location::empty(),
                                        _type: arc::new(Type::Error),
                                        origin: String::with_capacity(0),
                                        arity: 0,
                                        tailrec: false,
                                        impure: false,
                                    },
                                    String::from("cons_S"),
                                )),
                                Box::new(SExpr::Char(
                                    SExprMetadata {
                                        loc: loc.clone(),
                                        loc2: Location::empty(),
                                        _type: arc::new(Type::Char),
                                        origin: String::with_capacity(0),
                                        arity: 0,
                                        tailrec: false,
                                        impure: false,
                                    },
                                    c,
                                )),
                            )),
                            Box::new(cons),
                        )
                    }
                    cons
                }
            }
        */
        // Infix
        Ast::Infix(span, op, l, r) => {
            if op == "$" {
                SExpr::Application(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: ArityInfo::Unknown,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
                    vec![convert_node(*r, filename, funcs, global, seen_funcs, types)],
                )
            } else {
                unreachable!("uwu moment");
            }
        }

        // Application
        Ast::Application(span, l, r) => SExpr::Application(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: ArityInfo::Unknown,
                tailrec: false,
                impure: false,
            },
            Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
            r.into_iter().map(|r| convert_node(r, filename, funcs, global, seen_funcs, types)).collect(),
        ),

        // Assignment
        Ast::Assign(span, name, val) => {
            let sexpr = convert_node(*val, filename, funcs, false, seen_funcs, types);
            if global && name != "_" {
                let func_name = if seen_funcs.contains_key(&name) {
                    let seen = seen_funcs.get_mut(&name).unwrap();
                    let name = format!("{}.{}", name, seen);
                    *seen += 1;
                    name
                } else {
                    seen_funcs.insert(name.clone(), 0);
                    name.clone()
                };
                funcs.insert(
                    func_name.clone(),
                    IrFunction {
                        loc: Location::new(span.clone(), filename),
                        name: name.clone(),
                        _type: arc::new(Type::Unknown),
                        args: Vec::with_capacity(0),
                        captured: HashMap::with_capacity(0),
                        captured_names: Vec::with_capacity(0),
                        body: sexpr,
                        global: true,
                        checked: false,
                        written: false,
                        impure: false,
                    },
                );

                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span.clone(), filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: ArityInfo::Unknown,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(SExpr::Function(
                        SExprMetadata {
                            loc: Location::new(span, filename),
                            loc2: Location::empty(),
                            origin: String::with_capacity(0),
                            _type: arc::new(Type::Error),
                            arity: ArityInfo::Unknown,
                            tailrec: false,
                            impure: false,
                        },
                        func_name,
                    )),
                )
            } else {
                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: ArityInfo::Unknown,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(sexpr),
                )
            }
        }

        // Assignment with types
        Ast::AssignTyped(span, name, _type, val) => {
            let sexpr = convert_node(*val, filename, funcs, false, seen_funcs, types);
            if global && name != "_" {
                let func_name = if seen_funcs.contains_key(&name) {
                    let seen = seen_funcs.get_mut(&name).unwrap();
                    let name = format!("{}.{}", name, seen);
                    *seen += 1;
                    name
                } else {
                    seen_funcs.insert(name.clone(), 0);
                    name.clone()
                };
                funcs.insert(
                    func_name.clone(),
                    IrFunction {
                        loc: Location::new(span.clone(), filename),
                        name: name.clone(),
                        _type: arc::new(Type::Unknown),
                        args: Vec::with_capacity(0),
                        captured: HashMap::with_capacity(0),
                        captured_names: Vec::with_capacity(0),
                        body: sexpr,
                        global: true,
                        checked: false,
                        written: false,
                        impure: false,
                    },
                );

                let ts = _type.get_span();
                let _type = arc::new(types::convert_ast_to_type(*_type, filename));
                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span.clone(), filename),
                        loc2: Location::new(ts, filename),
                        origin: String::with_capacity(0),
                        _type: _type.clone(),
                        arity: ArityInfo::Unknown,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(SExpr::Function(
                        SExprMetadata {
                            loc: Location::new(span, filename),
                            loc2: Location::empty(),
                            origin: String::with_capacity(0),
                            _type,
                            arity: ArityInfo::Unknown,
                            tailrec: false,
                            impure: false,
                        },
                        func_name,
                    )),
                )
            } else {
                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::new(_type.get_span(), filename),
                        origin: String::with_capacity(0),
                        _type: arc::new(types::convert_ast_to_type(*_type, filename)),
                        arity: ArityInfo::Unknown,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(sexpr),
                )
            }
        }

        Ast::AssignType(span, name, _type) => {
            let span2 = _type.get_span();
            let _type = arc::new(types::convert_ast_to_type(*_type, filename));
            types.insert(name.clone(), _type.clone());
            SExpr::TypeAlias(
                SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::new(span2, filename),
                    origin: String::with_capacity(0),
                    _type,
                    arity: ArityInfo::Unknown,
                    tailrec: false,
                    impure: false,
                },
                name,
            )
        }

        // Assigning functions
        Ast::AssignFunction(span, name, args, val) => {
            // Get function id
            let func_name = if seen_funcs.contains_key(&name) {
                let seen = seen_funcs.get_mut(&name).unwrap();
                let name = format!("{}.{}", name, seen);
                *seen += 1;
                name
            } else {
                seen_funcs.insert(name.clone(), 0);
                name.clone()
            };

            let func_id = SExpr::Function(
                SExprMetadata {
                    loc: Location::new(span.clone(), filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type: arc::new(Type::Error),
                    arity: ArityInfo::Unknown,
                    tailrec: false,
                    impure: false,
                },
                func_name.clone(),
            );

            // Create the function
            let func = IrFunction {
                loc: Location::new(
                    Span {
                        start: span.start,
                        end: func_id.get_metadata().loc.span.start,
                    },
                    filename,
                ),
                name: name.clone(),
                _type: arc::new(Type::Unknown),
                args: args
                    .into_iter()
                    .map(|v| (v.0, arc::new(types::convert_ast_to_type(v.1, filename))))
                    .collect(),
                captured: HashMap::with_capacity(0),
                captured_names: Vec::with_capacity(0),
                body: convert_node(*val, filename, funcs, false, seen_funcs, types),
                global,
                checked: false,
                written: false,
                impure: false,
            };

            let mut _type = arc::new(Type::Error);
            for a in func.args.iter() {
                if let Type::UndeclaredTypeError(_) = *a.1 {
                    _type = a.1.clone();
                    break;
                } else if let Type::DuplicateTypeError(_, _, _) = *a.1 {
                    _type = a.1.clone();
                    break;
                }
            }

            // Return assigning to the function id
            funcs.insert(func_name, func);
            SExpr::Assign(
                SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type,
                    arity: ArityInfo::Unknown,
                    tailrec: false,
                    impure: false,
                },
                name,
                Box::new(func_id),
            )
        }

        Ast::Lambda(span, args, val) => {
            // Get function id
            let func_name = {
                let seen = seen_funcs.get_mut("").unwrap();
                let name = format!(".{}", seen);
                *seen += 1;
                name
            };

            let mut func_id = SExpr::Function(
                SExprMetadata {
                    loc: Location::new(span.clone(), filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type: arc::new(Type::Error),
                    arity: ArityInfo::Unknown,
                    tailrec: false,
                    impure: false,
                },
                func_name.clone(),
            );

            // Create the function
            let func = IrFunction {
                loc: Location::new(span, filename),
                name: func_name.clone(),
                _type: arc::new(Type::Unknown),
                args: args
                    .into_iter()
                    .map(|v| (v.0, arc::new(types::convert_ast_to_type(v.1, filename))))
                    .collect(),
                captured: HashMap::with_capacity(0),
                captured_names: Vec::with_capacity(0),
                body: convert_node(*val, filename, funcs, false, seen_funcs, types),
                global: false,
                checked: false,
                written: false,
                impure: false,
            };

            let mut _type = arc::new(Type::Error);
            for a in func.args.iter() {
                if let Type::UndeclaredTypeError(_) = *a.1 {
                    _type = a.1.clone();
                    break;
                } else if let Type::DuplicateTypeError(_, _, _) = *a.1 {
                    _type = a.1.clone();
                    break;
                }
            }

            // Return the function id
            funcs.insert(func_name, func);
            func_id.get_mutable_metadata()._type = _type;
            func_id
        }

        // With expressions
        Ast::With(span, a, v) => SExpr::With(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: ArityInfo::Unknown,
                tailrec: false,
                impure: false,
            },
            a.into_iter()
                .map(|a| convert_node(a, filename, funcs, false, seen_funcs, types))
                .collect(),
            Box::new(convert_node(*v, filename, funcs, false, seen_funcs, types)),
        ),

        /*
        Ast::Walrus(span, a, v) => SExpr::Walrus(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                tailrec: false,
                impure: false,
            },
            a,
            Box::new(convert_node(*v, filename, funcs, false, seen_funcs, types)),
        ),
        */
        Ast::Match(span, v, a) => SExpr::Match(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: ArityInfo::Unknown,
                tailrec: false,
                impure: false,
            },
            Box::new(convert_node(*v, filename, funcs, global, seen_funcs, types)),
            a.into_iter()
                .map(|a| {
                    let span2 = a.0.get_span();
                    (
                        arc::new(types::convert_ast_to_type(a.0, filename)),
                        convert_node(a.1, filename, funcs, global, seen_funcs, types),
                        Location::new(span2, filename),
                    )
                })
                .collect(),
        ),

        Ast::Int(_, _) => todo!(),
        Ast::Float(_, _) => todo!(),
        Ast::Word(_, _) => todo!(),
        Ast::Char(_, _) => todo!(),
        Ast::String(_, _) => todo!(),
        Ast::Enum(_, _) => todo!(),
        Ast::List(_, _) => todo!(),
        Ast::Prefix(_, _, _) => todo!(),
        Ast::As(_, _, _) => todo!(),
        Ast::Walrus(_, _, _) => todo!(),
    }
}

// extract_types_to_ir(&Vec<Ast>, &mut IrModule) -> ()
// Extracts types and inserts them into the Ir's list of types.
fn extract_types_to_ir(asts: &[Ast], module: &mut IrModule) {
    for ast in asts {
        if let Ast::AssignType(_, v, _) = ast {
            module.types.insert(v.clone(), arc::new(Type::Unknown));
        }
    }
}

// Represents the purity of the next function.
enum Purity {
    Pure,
    Impure,
    Default,
}

// convert_ast_to_ir(Vec<Ast>) -> Ir
// Converts a list of asts into ir.
pub fn convert_ast_to_ir(
    filename: &str,
    contents: &str,
    asts: Vec<Ast>,
    ir: &mut Ir,
) -> Result<(), Vec<IrError>> {
    // Set up
    let mut module = IrModule::new(filename, contents);
    extract_types_to_ir(&asts, &mut module);
    let mut seen_funcs = HashMap::new();
    seen_funcs.insert(String::with_capacity(0), 0);
    let mut module_name = String::with_capacity(0);
    let mut errors = vec![];
    let mut purity = Purity::Default;

    // Iterate over every ast node
    for ast in asts {
        // Deal with the header
        if let Ast::Header(_, name, _exports, imports) = ast {
            // Get module name
            let mut full_name = vec![];
            let mut top = *name;
            while let Ast::Infix(_, _, l, r) = top {
                if let Ast::Symbol(_, v) = *r {
                    full_name.push(v);
                }

                top = *l;
            }
            if let Ast::Symbol(_, v) = top {
                full_name.push(v);
            }
            full_name.reverse();
            module_name = full_name.join("::");

            // Deal with exports
            /*
            for export in exports {
                // Check exported variable type
                if let Ast::Empty = export.2 {
                    unimplemented!("nya :(");
                } else {
                    let _type = types::convert_ast_to_type(export.2, filename);
                    if let Type::UndeclaredTypeError(s) = _type {
                        errors.push(IrError::InvalidType(s));
                    } else if let Type::DuplicateTypeError(s1, s2, t) = _type {
                        errors.push(IrError::DuplicateTypeInUnion(s1, s2, t));

                    // Check export is unique
                    } else if module.exports.contains_key(&export.1) {
                        errors.push(IrError::DoubleExport(
                            module.exports.get(&export.1).unwrap().0.clone(),
                            Location::new(export.0, filename),
                            export.1,
                        ));
                    } else {
                        // Add export to list of exports
                        module.exports.insert(
                            export.1,
                            (Location::new(export.0, filename), arc::new(_type)),
                        );
                    }
                }
            }*/

            // Deal with imports
            for import in imports {
                let imp_mod;
                let alias;
                if let Ast::QualifiedImport(s, m, a) = import {
                    let mut name = vec![];
                    let mut m = m;
                    while let Ast::Infix(_, _, l, r) = *m {
                        if let Ast::Symbol(_, v) = *r {
                            name.push(v);
                        } else {
                            unreachable!("always a symbol");
                        }
                        m = l;
                    }
                    if let Ast::Symbol(_, v) = *m {
                        name.push(v);
                    }
                    name.reverse();

                    if !a.is_empty() {
                        alias = a;
                    } else {
                        alias = name.join("::");
                    }

                    imp_mod = IrImport {
                        name: name.join("::"),
                        loc: Location::new(s, filename),
                        qualified: true,
                        imports: HashMap::with_capacity(0),
                    };
                } else if let Ast::Import(s, m, imports) = import {
                    let mut name = vec![];
                    let mut m = m;
                    while let Ast::Infix(_, _, l, r) = *m {
                        if let Ast::Symbol(_, v) = *r {
                            name.push(v);
                        } else {
                            unreachable!("always a symbol");
                        }
                        m = l;
                    }
                    if let Ast::Symbol(_, v) = *m {
                        name.push(v);
                    }
                    name.reverse();

                    alias = name.join("::");
                    imp_mod = IrImport {
                        name: name.join("::"),
                        loc: Location::new(s, filename),
                        qualified: false,
                        imports: imports
                            .into_iter()
                            .map(|v| (v, (arc::new(Type::Unknown), 0, false)))
                            .collect(),
                    };
                } else {
                    unreachable!("always either a QualifiedImport or an Import");
                }

                match module.imports.entry(alias) {
                    Entry::Occupied(e) => {
                        errors.push(IrError::RedefineImportAlias(
                            e.get().loc.clone(),
                            imp_mod.loc,
                            e.key().clone(),
                        ));
                    }

                    Entry::Vacant(e) => {
                        e.insert(imp_mod);
                    }
                }
            }
        } else if let Ast::Annotation(span, a) = ast {
            // Purity tags
            if a == "@pure" {
                purity = Purity::Pure;
            } else if a == "@impure" {
                purity = Purity::Impure;
            } else {
                errors.push(IrError::UnsupportedAnnotation(
                    Location::new(span, filename),
                    a,
                ));
            }
        } else if let Ast::Extern(span, c, n, t) = ast {
            // let ts = t.get_span().clone();
            let t = types::convert_ast_to_type(*t, &module.filename);

            // Check type
            if let Type::UndeclaredTypeError(s) = t {
                errors.push(IrError::InvalidType(s));
            } else if let Type::DuplicateTypeError(s1, s2, t2) = t {
                errors.push(IrError::DuplicateTypeInUnion(s1, s2, t2));
            } else {
                // Get arg types and return function
                let mut arg_types = vec![];
                let mut ret_type = arc::new(t);

                while let Type::Func(f, a) = &*ret_type {
                    arg_types.push(f.clone());
                    ret_type = a.clone();
                }

                // Add external function
                module.externals.insert(
                    n,
                    IrExtern {
                        loc: Location::new(span, &module.filename),
                        extern_name: c,
                        arg_types,
                        ret_type,
                        impure: matches!(purity, Purity::Default | Purity::Impure),
                    },
                );
            }

            purity = Purity::Default;
        } else {
            let v = convert_node(
                ast,
                filename,
                &mut module.funcs,
                true,
                &mut seen_funcs,
                &mut module.types,
            );

            if let SExpr::Assign(_, a, v) = v {
                if let SExpr::Function(_, f) = *v {
                    module.funcs.get_mut(&f).unwrap().impure = matches!(purity, Purity::Impure);
                    module.globals.insert(a, f);
                }
            }
            purity = Purity::Default;
        }
    }

    // Check module name
    if module_name.is_empty() {
        module_name = filename
            .split('/')
            .last()
            .unwrap()
            .split('.')
            .next()
            .unwrap()
            .to_string();
    }
    module.name = module_name.clone();

    // Add module to ir root and error if already exists
    match ir.modules.entry(module_name) {
        Entry::Occupied(e) => {
            if format!("{:?}", module) == format!("{:?}", e.get()) {
            } else {
                errors.push(IrError::DuplicateModule(
                    e.key().clone(),
                    DuplicateModuleInfo::NoSuperset,
                ));
            }
        }

        Entry::Vacant(e) => {
            e.insert(module);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/*
pub fn convert_library_header(
    filename: &str,
    asts: Vec<Ast>,
    ir: &mut Ir,
) -> Result<(), Vec<IrError>> {
    let mut errors = vec![];

    for ast in asts {
        errors.extend(convert_module(filename, ast, ir));
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn convert_module(filename: &str, ast: Ast, ir: &mut Ir) -> Vec<IrError> {
    let mut errors = vec![];

    if let Ast::LibHeader(_, name, exports) = ast {
        // Get module name
        let mut full_name = vec![];
        let mut top = *name;
        while let Ast::Infix(_, _, l, r) = top {
            if let Ast::Symbol(_, v) = *r {
                full_name.push(v);
            }

            top = *l;
        }
        if let Ast::Symbol(_, v) = top {
            full_name.push(v);
        }
        full_name.reverse();
        let module_name = full_name.join("::");
        let mut module = IrModule::new(filename, "");
        module.name = module_name.clone();
        module.lib = true;

        // Deal with exports
        for export in exports {
            // Check exported variable type
            if let Ast::Empty = export.4 {
                unimplemented!("nya :(");
            } else {
                let _type = arc::new(types::convert_ast_to_type(export.4, filename));
                if let Type::DuplicateTypeError(s1, s2, t) = &*_type {
                    errors.push(IrError::DuplicateTypeInUnion(
                        s1.clone(),
                        s2.clone(),
                        t.clone(),
                    ));

                // Check export is unique
                } else {
                    let (span, export, arity, impure, _) = export;
                    match module.exports.entry(export) {
                        Entry::Occupied(e) => {
                            errors.push(IrError::DoubleExport(
                                e.get().0.clone(),
                                Location::new(span, filename),
                                e.key().clone(),
                            ));
                        }

                        Entry::Vacant(e) => {
                            // Add export to list of exports
                            let loc = Location::new(span, filename);
                            module
                                .scope
                                .put_var(e.key(), &_type, &loc, true, &module_name);
                            let mut args = vec![];
                            let mut ret_type = _type.clone();
                            for i in 0..arity {
                                if let Type::Func(l, r) = &*ret_type {
                                    args.push((format!("${}", i), l.clone()));
                                    ret_type = r.clone();
                                }
                            }

                            module.funcs.insert(
                                e.key().clone(),
                                IrFunction {
                                    loc: Location::empty(),
                                    name: e.key().clone(),
                                    _type: arc::new(Type::Unknown),
                                    args,
                                    captured: HashMap::with_capacity(0),
                                    captured_names: Vec::with_capacity(0),
                                    body: SExpr::Empty(SExprMetadata {
                                        loc: Location::empty(),
                                        loc2: Location::empty(),
                                        origin: String::with_capacity(0),
                                        _type: ret_type.clone(),
                                        tailrec: false,
                                        impure,
                                    }),
                                    global: true,
                                    checked: true,
                                    written: true,
                                    impure,
                                },
                            );
                            module.globals.insert(e.key().clone(), e.key().clone());
                            e.insert((loc, _type));
                        }
                    }
                }
            }
        }

        // Add module to ir root and error if already exists
        match ir.modules.entry(module_name) {
            Entry::Occupied(mut e) => {
                let old_mod = e.get();
                let new_mod = &module;

                let mut new_superset_of_old = true;
                let mut old_superset_of_new = true;

                for i in old_mod.exports.keys() {
                    let old_mod_export = old_mod.exports.get(i).unwrap();

                    let new_mod_export = match new_mod.exports.get(i) {
                        Some(v) => v,
                        _ => {
                            new_superset_of_old = false;
                            break;
                        }
                    };

                    if old_mod_export.1 != new_mod_export.1 {
                        new_superset_of_old = false;
                        break;
                    }
                }

                for i in new_mod.exports.keys() {
                    let new_mod_export = new_mod.exports.get(i).unwrap();

                    let old_mod_export = match old_mod.exports.get(i) {
                        Some(v) => v,
                        _ => {
                            old_superset_of_new = false;
                            break;
                        }
                    };

                    if new_mod_export.1 != old_mod_export.1 {
                        old_superset_of_new = false;
                        break;
                    }
                }

                if new_superset_of_old && old_superset_of_new {
                    errors.push(IrError::DuplicateModule(
                        e.key().clone(),
                        DuplicateModuleInfo::BothSuperset,
                    ));
                } else if new_superset_of_old ^ old_superset_of_new {
                    //TODO: Duplicate module superset warning
                    if new_superset_of_old {
                        e.insert(module);
                        errors.push(IrError::DuplicateModule(
                            e.key().clone(),
                            DuplicateModuleInfo::NewSupersetOld,
                        ));
                    } else {
                        errors.push(IrError::DuplicateModule(
                            e.key().clone(),
                            DuplicateModuleInfo::OldSupersetNew,
                        ));
                    }
                } else {
                    errors.push(IrError::DuplicateModule(
                        e.key().clone(),
                        DuplicateModuleInfo::NoSuperset,
                    ));
                }
            }

            Entry::Vacant(e) => {
                e.insert(module);
            }
        }
    }

    errors
}
*/
