// #![deny(missing_docs)]

/// Module that contains helper functions for transforming the higher level intermediate
/// representation into machine code. This includes functions for lowering the IR, functions for
/// manipulating the lower level IR, functions for manipulating code structures, and functions for
/// emitting code.
pub mod backends;

/// Module that contains helper functions transforming the source text into higher level
/// intermediate representation. This includes functions for parsing, functions for transforming
/// the text into IR, and functions for checking the correctness of IR.
pub mod frontend;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use logos::Span;
use std::collections::HashMap;

use crate::frontend::ir::{self, Ir, IrError};
use crate::frontend::parser;

/// Determines whether the compiler should output debug information or not.
static DEBUG: bool = false;

/// The return type of check<>().
pub type Res<'a> = Result<
    (Vec<Diagnostic<usize>>, SimpleFiles<&'a String, String>),
    (Vec<Diagnostic<usize>>, SimpleFiles<&'a String, String>),
>;

/// Checks whether given code is valid.
pub fn check<'a>(
    filenames: &'a [(String, bool)],
    codes: &[String],
    ir: &mut Ir,
    _require_main: bool,
    emit: bool,
) -> Res<'a> {
    // Set up codespan
    let mut files = SimpleFiles::new();
    let mut file_hash = HashMap::new();
    for file in filenames.iter().enumerate() {
        file_hash.insert(&file.1 .0, files.add(&file.1 .0, codes[file.0].clone()));
    }
    let file_hash = file_hash;

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let mut diagnostics = Vec::new();
    let mut fail = false;

    for (file, code) in filenames.iter().zip(codes.iter()) {
        let file_id = *file_hash.get(&file.0).unwrap();

        if let Some(start) = code.find("uwu") {
            let loc = Span {
                start,
                end: start + 3,
            };
            let diagnostic = Diagnostic::note()
                .with_message("owo")
                .with_labels(vec![Label::primary(file_id, loc).with_message("nya")]);
            if emit {
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            }
            diagnostics.push(diagnostic);
        }

        // Generate the ast
        if file.1 {
            // TODO: remove this condition
            todo!("this should never be available");
        } else {
            let ast = match parser::parse(code) {
                Ok(v) => v,
                Err(e) => {
                    let diagnostic = Diagnostic::error()
                        .with_message(&e.msg)
                        .with_labels(vec![Label::primary(file_id, e.span)]);
                    if emit {
                        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                    }
                    diagnostics.push(diagnostic);
                    return Err((diagnostics, files));
                }
            };

            // Print out the ast
            if DEBUG {
                println!("{:#?}", &ast);
            }
            match ir::convert_ast_to_ir(&file.0, code, ast, ir) {
                Ok(_) if DEBUG => {
                    dbg!(&ir);
                }
                Ok(_) => (),
                Err(e) => {
                    for e in e {
                        let mut diagnostic = Diagnostic::error();
                        match e {
                            IrError::InvalidType(s) => {
                                diagnostic = diagnostic
                                    .with_message("Invalid type used")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message("Undeclared type")])
                            }

                            IrError::DuplicateTypeInUnion(s1, s2, t) => {
                                diagnostic = diagnostic
                                    .with_message("Duplicate type in union type declaration")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Type used here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Type `{}` used a second time here", t),
                                        ),
                                    ])
                            }

                            IrError::DoubleExport(s1, s2, e) => {
                                diagnostic = diagnostic
                                    .with_message("Value exported twice")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Value exported here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Value {} exported a second time here", e),
                                        ),
                                    ])
                            }

                            IrError::RedefineImportAlias(s1, s2, a) => {
                                diagnostic = diagnostic
                                    .with_message("Alias defined twice")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Alias defined here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Alias {} defined a second time here", a),
                                        ),
                                    ])
                            }

                            IrError::UnsupportedAnnotation(s, a) => {
                                diagnostic = diagnostic
                                    .with_message("Unsupported annotation used")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message(format!("Annotation {} is unsupported", a))])
                            }

                            IrError::InvalidFFIType(s, t) => {
                                diagnostic = diagnostic
                                    .with_message("Unsupported type used for FFI")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message(format!("Type {} is unsupported by FFI", t))])
                            }

                            IrError::DuplicateModule(v, _t) => {
                                diagnostic =
                                    diagnostic.with_message(format!("Duplicate module `{}`", v))
                            }
                        }
                        if emit {
                            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                        }
                        diagnostics.push(diagnostic);
                        fail = true;
                    }
                }
            }
        }
    }

    if fail {
        Err((diagnostics, files))
    } else {
        Ok((diagnostics, files))
    }
}
