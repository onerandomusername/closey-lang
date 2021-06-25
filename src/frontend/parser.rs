use logos::{Lexer, Logos, Span};

// convert_chars(&str) -> String
// Converts escaped characters into an unescaped string.
fn convert_chars(s: &str, off: usize) -> String {
    let mut iter = s[off..s.len() - off].chars();
    let mut s = String::new();

    while let Some(c) = iter.next() {
        if c == '\\' {
            match iter.next().unwrap() {
                '\\' => s.push('\\'),
                '\"' => s.push('\"'),
                '\'' => s.push('\''),
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                '0' => s.push('\0'),
                c => {
                    s.push('\\');
                    s.push(c)
                }
            };
        } else {
            s.push(c);
        }
    }

    s
}

// The tokens parsed by the lexer.
#[derive(Logos, PartialEq, Debug, Clone)]
pub enum Token {
    // Brackets
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    // Whitespace
    #[token("\n")]
    Newline,

    #[regex(r"([ \t\f\r]|\\\n)+", logos::skip)]
    Whitespace,

    #[regex(r"#[^\n]*", logos::skip)]
    #[regex(r"\{-([^-]*-+)+\}", logos::skip)]
    Comment,

    // Error
    #[error]
    Error,

    // Punctuation and symbols
    #[token(":")]
    Colon,

    #[token("::")]
    ColonColon,

    #[token(",")]
    Comma,

    #[token("\\")]
    Backslash,

    #[token(".")]
    Dot,

    #[token("$")]
    Dollar,

    #[token(";")]
    Semicolon,

    #[token("|")]
    Bar,

    #[token("=")]
    Assign,

    #[regex(r"(;~!\$%\^&\*\-\+|\./\?)+")]
    Operator,

    // Numbers
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16))]
    #[regex(r"0b[01]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2))]
    Int(i64),

    #[regex(r"[0-9]+(\.[0-9]*([eE][+-]?[0-9]+)?|[eE][+-]?[0-9]+)", |lex| lex.slice().parse())]
    Float(f64),

    #[regex(r"[0-9]+u", |lex| {
        let v = lex.slice();
        v[..v.len() - 1].parse()
    })]
    #[regex(r"[a-fA-F0-9]+h", |lex| {
        let v = lex.slice();
        u64::from_str_radix(&v[..v.len() - 1], 16)
    })]
    #[regex(r"[01]+b", |lex| {
        let v = lex.slice();
        u64::from_str_radix(&v[..v.len() - 1], 2)
    })]
    Word(u64),

    #[regex(r#"'([^\\']|\\[nrt'"0])'"#, |lex| convert_chars(lex.slice(), 1).bytes().next().unwrap())]
    Char(u8),

    #[regex(r"'[a-zA-Z_0-9]+", |lex| lex.slice()[1..].to_owned())]
    Generic(String),

    // Symbols (variables and stuff)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_']*")]
    Symbol,

    // Annotations
    #[regex(r"@[a-z_]+")]
    Annotation,

    // Strings
    #[regex(r#""([^\\"]|\\.)*""#, |lex| convert_chars(lex.slice(), 1))]
    #[regex(r##"#"([^"]|"[^#])*"#"##, |lex| convert_chars(lex.slice(), 2))]
    String(String),

    // Arrows
    #[token("+>")]
    PlusArrow,

    #[token("->")]
    RightArrow,

    #[token("=>")]
    ThiccArrow,

    // Keywords
    #[token("let")]
    Let,

    #[token("in")]
    In,

    #[token("import")]
    Import,

    #[token("module")]
    Module,

    #[token("extern")]
    Extern,

    #[token("type")]
    Type,

    #[token("ptr")]
    Pointer,

    #[token("match")]
    Match,

    #[token("to")]
    To,

    Unreachable,
}

// Represents a parser.
struct Parser<'a> {
    // The lexer the parser uses internally.
    lexer: Lexer<'a, Token>,

    // The tokens already parsed
    tokens: Vec<(Token, Span)>,

    // The current position of the parser.
    token_pos: usize,
}

impl<'a> Parser<'a> {
    // new(&str) -> Parser
    // Creates a new parser
    fn new(s: &str) -> Parser {
        Parser {
            lexer: Token::lexer(s),
            tokens: vec![],
            token_pos: 0,
        }
    }

    // next(&mut self) -> Option<&(Token, Span)>
    // Gets the next token.
    fn next(&mut self) -> Option<&(Token, Span)> {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len() {
            let token = &self.tokens[self.token_pos];
            self.token_pos += 1;
            Some(token)

        // Otherwise get token from the lexer
        } else {
            self.tokens.push((self.lexer.next()?, self.lexer.span()));
            self.token_pos += 1;
            self.tokens.last()
        }
    }

    // peek(&mut self) -> Option<&(Token, Span)>
    // Peeks at the next token.
    fn peek(&mut self) -> Option<(&Token, Span)> {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len() {
            let token = &self.tokens[self.token_pos];
            Some((&token.0, token.1.clone()))

        // Otherwise get token from lexer
        } else {
            self.tokens.push((self.lexer.next()?, self.lexer.span()));
            let token = self.tokens.last()?;
            Some((&token.0, token.1.clone()))
        }
    }

    // slice(&self) -> String
    // Returns the slice corresponding to the current token.
    fn slice(&mut self) -> String {
        if self.token_pos >= self.tokens.len() {
            self.peek();
        }

        if self.token_pos < self.tokens.len() {
            let range = &self.tokens[self.token_pos].1;
            String::from(&self.lexer.source()[range.start..range.end])
        } else {
            String::with_capacity(0)
        }
    }

    // span(&self) -> Span
    // Returns the current span.
    fn span(&mut self) -> Span {
        if let Some((_, s)) = self.peek() {
            s
        } else {
            self.lexer.span()
        }
    }

    // save_state(&self) -> usize
    // Saves the current token position by returning it.
    fn save_state(&self) -> usize {
        self.token_pos
    }

    // return_state(&mut self, usize) -> ()
    // Returns to a given state.
    fn return_state(&mut self, state: usize) {
        self.token_pos = state;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Empty,

    // Numbers
    Int(Span, i64),
    Float(Span, f64),
    Word(Span, u64),
    Char(Span, u8),

    // String
    String(Span, String),

    // Symbol (variables and stuff)
    Symbol(Span, String),

    // Generic type ('a)
    Generic(Span, String),

    // Enum (ie, atoms)
    Enum(Span, String),

    // Annotations (@pure, @impure, @memoize, etc)
    Annotation(Span, String),

    // Lists
    List(Span, Vec<Ast>),

    // Function Application
    Application(Span, Box<Ast>, Box<Ast>),

    // Prefix expressions
    Prefix(Span, String, Box<Ast>),

    // Infix expressions
    Infix(Span, String, Box<Ast>, Box<Ast>),

    // Casting
    As(Span, Box<Ast>, Box<Ast>),

    // Assignments
    Assign(Span, String, Box<Ast>),

    // Assignments with types
    AssignTyped(Span, String, Box<Ast>, Box<Ast>),

    // Assignment of types
    AssignType(Span, String, Box<Ast>),

    // Assignment of functions
    AssignFunction(Span, String, Vec<(String, Ast)>, Box<Ast>),

    // Lambda functions
    Lambda(Span, Vec<(String, Ast)>, Box<Ast>),

    // Match expressions
    Match(Span, Box<Ast>, Vec<(Ast, Ast)>),

    // Scoping
    With(Span, Vec<Ast>, Box<Ast>),
    Walrus(Span, String, Box<Ast>),

    // Imports
    Import(Span, Box<Ast>, Vec<String>),
    QualifiedImport(Span, Box<Ast>, String),

    // Header
    Header(Span, Box<Ast>, Vec<(Span, String, Ast)>, Vec<Ast>),
    LibHeader(Span, Box<Ast>, Vec<(Span, String, usize, bool, Ast)>),

    // External functions
    Extern(Span, String, String, Box<Ast>),
}

impl Ast {
    pub fn get_span(&self) -> Span {
        match self {
            Self::Int(s, _)
            | Self::Float(s, _)
            | Self::Word(s, _)
            | Self::Char(s, _)
            | Self::String(s, _)
            | Self::List(s, _)
            | Self::Symbol(s, _)
            | Self::Generic(s, _)
            | Self::Enum(s, _)
            | Self::Annotation(s, _)
            | Self::Application(s, _, _)
            | Self::Prefix(s, _, _)
            | Self::Infix(s, _, _, _)
            | Self::As(s, _, _)
            | Self::Assign(s, _, _)
            | Self::AssignTyped(s, _, _, _)
            | Self::AssignType(s, _, _)
            | Self::AssignFunction(s, _, _, _)
            | Self::Match(s, _, _)
            | Self::Lambda(s, _, _)
            | Self::With(s, _, _)
            | Self::Walrus(s, _, _)
            | Self::Import(s, _, _)
            | Self::QualifiedImport(s, _, _)
            | Self::Header(s, _, _, _)
            | Self::LibHeader(s, _, _)
            | Self::Extern(s, _, _, _) => s.clone(),

            Self::Empty => panic!("uwu moment"),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
    fatal: bool,
}

impl ParseError {
    // empty<T>() -> Result<T, ParseError>
    // Creates an empty ParseError.
    fn empty<T>() -> Result<T, ParseError> {
        Err(ParseError {
            span: Span { start: 0, end: 0 },
            msg: String::with_capacity(0),
            fatal: false,
        })
    }
}

// call_func(ident, ident, ident) -> Result<Ast, ParseError>
// Calls a function and returns if an error was encountered.
macro_rules! call_func {
    ($func: ident, $parser: ident, $state: ident) => {
        match $func($parser) {
            Ok(v) => v,
            Err(e) => {
                $parser.return_state($state);
                return Err(e);
            }
        }
    };
}

// call_func_fatal(ident, ident, literal, literal, expr*) -> Result<Ast, ParseError>
// Calls a function and returns a fatal error if unsuccessful.
macro_rules! call_func_fatal
{
    ($func: ident, $parser: ident, $format: literal $(,$vs: expr),*) => {
        match $func($parser)
        {
            Ok(v) => v,
            Err(e) if e.fatal => return Err(e),
            Err(_) => return Err(ParseError {
                span: $parser.span(),
                msg: format!($format $(,$vs),*),
                fatal: true
            })
        }
    }
}

// call_optional(ident, ident) => Result<Ast, ParseError>
// Calls a function and only returns if a fatal error is encountered.
macro_rules! call_optional {
    ($func: ident, $parser: ident) => {
        match $func($parser) {
            Ok(v) => Ok(v),
            Err(e) if e.fatal => return Err(e),
            Err(e) => Err(e),
        }
    };
}

// consume_nosave(ident, ident, ident, literal, literal, literal, expr*) -> Result<Ast, ParseError>
// Consumes a token without saving it, returning if an error was encountered.
macro_rules! consume_nosave
{
    ($parser: ident, $token: ident, $state: ident, $fatal: literal, $format: literal $(,$vs: expr),*) => {
        match $parser.peek()
        {
            Some((Token::$token, _)) => {
                $parser.next();
            }

            _ => {
                let span = $parser.span();
                $parser.return_state($state);
                return Err(ParseError {
                    span,
                    msg: format!($format $(,$vs),*),
                    fatal: $fatal
                });
            }
        }
    }
}

// consume_save(ident, ident, ident, literal, literal, literal, expr*) -> Result<Ast, ParseError>
// Consumes a token and saves it, returning if an error was encountered.
macro_rules! consume_save
{
    ($parser: ident, $token: ident, $state: ident, $fatal: literal, $format: literal $(,$vs: expr),*) => {
        match $parser.peek()
        {
            Some((Token::$token, s)) => {
                let v = ($parser.slice(), s);
                $parser.next();
                v
            }

            _ => {
                let span = $parser.span();
                $parser.return_state($state);
                return Err(ParseError {
                    span,
                    msg: format!($format $(,$vs),*),
                    fatal: $fatal
                })
            }
        };
    }
}

// infixl_op(ident, ident, pat, pat) -> Result<Ast, ParseError>
// Parses a left associative infix operator.
macro_rules! infixl_op {
    ($parser: ident, $subfunc: ident, $op1: pat, $op2: pat) => {{
        // Set up
        let state = $parser.save_state();
        let mut left = call_func!($subfunc, $parser, state);

        loop {
            // Save current state
            let state2 = $parser.save_state();
            newline($parser);

            // Check for operator
            if let Some(op) = $parser.peek() {
                // Get operator
                let op = match op.0 {
                    $op1 | $op2 => String::from($parser.slice()),
                    _ => {
                        $parser.return_state(state2);
                        break;
                    }
                };
                $parser.next();
                newline($parser);

                // Get right hand side
                let right =
                    call_func_fatal!($subfunc, $parser, "Expected value after infix operator");

                // Build ast
                left = Ast::Infix(
                    Span {
                        start: left.get_span().start,
                        end: right.get_span().end,
                    },
                    op,
                    Box::new(left),
                    Box::new(right),
                );

            // If there's no operator, break
            } else {
                break;
            }
        }

        Ok(left)
    }};
}

// infixl_op(ident, ident, pat, pat) -> Result<Ast, ParseError>
// Parses a right associative infix operator.
macro_rules! infixr_op {
    ($parser: ident, $subfunc: ident, $op1: pat, $op2: pat) => {{
        // Set up
        use std::mem::swap;
        let state = $parser.save_state();
        let mut top = call_func!($subfunc, $parser, state);
        let mut acc = &mut top;
        let mut first = true;
        let mut last = Span { start: 0, end: 0 };

        loop {
            // Save current state
            let state2 = $parser.save_state();
            newline($parser);

            // Check for operator
            if let Some(op) = $parser.peek() {
                // Get operator
                let op = match op.0 {
                    $op1 | $op2 => String::from($parser.slice()),
                    _ => {
                        $parser.return_state(state2);
                        break;
                    }
                };
                $parser.next();
                newline($parser);

                // Get right hand side
                let right =
                    call_func_fatal!($subfunc, $parser, "Expected value after infix operator");
                last = right.get_span();

                #[allow(unused_assignments)]
                if first {
                    let mut t1 = Ast::Empty;
                    let mut t2 = Ast::Empty;
                    acc = &mut t1;
                    swap(&mut t2, &mut top);
                    top = Ast::Infix(
                        Span {
                            start: t2.get_span().start,
                            end: right.get_span().end,
                        },
                        op,
                        Box::new(t2),
                        Box::new(right),
                    );
                    first = false;
                    acc = &mut top;
                } else {
                    let mut t = Ast::Empty;
                    if let Ast::Infix(_, _, _, r) = acc {
                        let r1 = &mut **r;
                        swap(r1, &mut t);
                        let ast = Ast::Infix(
                            Span {
                                start: t.get_span().start,
                                end: right.get_span().end,
                            },
                            op,
                            Box::new(t),
                            Box::new(right),
                        );
                        *r1 = ast;
                        acc = r1;
                    }
                }

            // If there's no operator, break
            } else {
                break;
            }
        }

        acc = &mut top;
        if !first {
            while let Ast::Infix(s, _, _, r) = acc {
                s.end = last.end;
                acc = r;
            }
        }

        Ok(top)
    }};
}

// newline(&mut Parser) -> ()
// Optionally parses newlines.
fn newline(parser: &mut Parser) {
    while let Some((Token::Newline, _)) = parser.peek() {
        parser.next();
    }
}

// symbol(&mut Parser) -> Result<Ast, ParseError>
// Parses a symbol.
fn symbol(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let (token, span) = consume_save!(parser, Symbol, state, false, "");
    Ok(Ast::Symbol(span, token))
}

// access_member(&mut Parser) -> Result<Ast, ParseError>
// Parses accessing a member.
fn access_member(parser: &mut Parser) -> Result<Ast, ParseError> {
    infixl_op!(parser, symbol, Token::ColonColon, Token::Unreachable)
}

// value(&mut Parser) -> Result<Ast, ParseError>
// Gets the next value.
fn value(parser: &mut Parser) -> Result<Ast, ParseError> {
    // Parse symbols/accessing members
    if let Ok(v) = call_optional!(access_member, parser) {
        return Ok(v);
    }

    // Get token
    let (token, _span) = match parser.peek() {
        Some(v) => v,
        None => return ParseError::empty(),
    };

    /*
    // Check for int
    if let Token::Int(n) = token {
        let n = *n;
        parser.next();
        Ok(Ast::Int(span, n))

    // Check for float
    } else if let Token::Float(n) = token {
        let n = *n;
        parser.next();
        Ok(Ast::Float(span, n))

    // Check for word
    } else if let Token::Word(n) = token {
        let n = *n;
        parser.next();
        Ok(Ast::Word(span, n))

    // Check for char
    } else if let Token::Char(c) = token {
        let c = *c;
        parser.next();
        Ok(Ast::Char(span, c))

    // Check for string
    } else if let Token::String(s) = token {
        let s = s.clone();
        parser.next();
        Ok(Ast::String(span, s))

    // Check for enum
    } else if let Token::Enum = token {
        let s = parser.span();
        let state = parser.save_state();
        parser.next();
        let (t, s2) = consume_save!(parser, Symbol, state, true, "");
        Ok(Ast::Enum(
            Span {
                start: s.start,
                end: s2.end,
            },
            t,
        ))

    // True
    } else if let Token::True = token {
        parser.next();
        Ok(Ast::True(span))

    // False
    } else if let Token::False = token {
        parser.next();
        Ok(Ast::False(span))

    // Parenthesised expressions
    } else */
    if let Token::LParen = token {
        // Get value
        let state = parser.save_state();
        parser.next();
        newline(parser);
        let value = match expression(parser) {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get right parenthesis
        newline(parser);
        consume_nosave!(parser, RParen, state, true, "");
        Ok(value)

    // Not a value
    } else {
        ParseError::empty()
    }
}

fn _as(parser: &mut Parser) -> Result<Ast, ParseError> {
    let value = value(parser)?;

    if let Some((Token::Colon, _)) = parser.peek() {
        parser.next();
        let _type = call_func_fatal!(type_expr, parser, "Expected type after `:`");

        Ok(Ast::As(
            Span {
                start: value.get_span().start,
                end: _type.get_span().end,
            },
            Box::new(value),
            Box::new(_type),
        ))
    } else {
        Ok(value)
    }
}

// application(&mut Parser) -> Result<Ast, ParseError>
// Parses function application.
fn application(parser: &mut Parser) -> Result<Ast, ParseError> {
    let mut left = _as(parser)?;

    loop {
        let right = match _as(parser) {
            Ok(v) => v,
            Err(e) if e.fatal => break Err(e),
            Err(_) => break Ok(left),
        };

        left = Ast::Application(
            Span {
                start: left.get_span().start,
                end: right.get_span().end,
            },
            Box::new(left),
            Box::new(right),
        );
    }
}

// list(&mut Parser) -> Result<Ast, ParseError>
// Parses a list.
fn list(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let (_, start) = consume_save!(parser, LBrack, state, false, "");
    let mut list = vec![];

    loop {
        if !list.is_empty() {
            match parser.peek() {
                Some((Token::Comma, _)) => {
                    parser.next();
                }

                _ => break,
            }
        }

        newline(parser);
        list.push(match expression(parser) {
            Ok(v) => v,
            Err(e) if e.fatal => return Err(e),
            Err(_) => break,
        });
    }

    newline(parser);
    let (_, end) = consume_save!(
        parser,
        RBrack,
        state,
        true,
        "Expected `]` after end of list"
    );

    Ok(Ast::List(
        Span {
            start: start.start,
            end: end.end,
        },
        list,
    ))
}

// lambda(&mut Parser) -> Result<Ast, ParseError>
// Parses a lambda function.
fn lambda(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let mut args = vec![];
    let (_, span) = consume_save!(parser, Backslash, state, false, "");

    // Get arguments
    loop {
        // Get comma
        if !args.is_empty() {
            match parser.peek() {
                Some((Token::Comma, _)) => {
                    parser.next();
                }

                _ => break,
            }
        }

        let arg = match declaration(parser) {
            Ok(v) => (v.1, v.2),
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        args.push(arg);
    }

    // Check that there is at least one argument
    if args.is_empty() {
        parser.return_state(state);
        return Err(ParseError {
            span: parser.span(),
            msg: String::from("Expected argument after `lambda`"),
            fatal: true,
        });
    }

    // Get the assign operator
    let slice = parser.slice();
    consume_nosave!(parser, Dot, state, true, "Expected `.`, got `{}`", slice);

    // Get the value
    newline(parser);
    let body = call_func_fatal!(apply_op, parser, "Expected function body after `=`");

    Ok(Ast::Lambda(
        Span {
            start: span.start,
            end: body.get_span().end,
        },
        args,
        Box::new(body),
    ))
}

// matchy(&mut Parser) -> Result<Ast, ParseError>
// Parses a match expression.
fn matchy(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let (_, span) = consume_save!(parser, Match, state, false, "");

    // Get value
    let value = call_func_fatal!(apply_op, parser, "Expected expression after `match`");
    let mut arms = vec![];
    newline(parser);

    while let Some((Token::To, _)) = parser.peek() {
        parser.next();
        let _type = call_func_fatal!(type_expr, parser, "Expected type after `to`");
        newline(parser);
        consume_nosave!(parser, ThiccArrow, state, true, "Expected `=>` after type");
        newline(parser);
        let value = call_func_fatal!(apply_op, parser, "Expected expression after `=>`");
        arms.push((_type, value));
        newline(parser);
    }

    // Error if no match arms
    if arms.is_empty() {
        return Err(ParseError {
            span: Span {
                start: span.start,
                end: value.get_span().end,
            },
            msg: String::from("Expected `to` after match value"),
            fatal: true,
        });
    }

    Ok(Ast::Match(
        Span {
            start: span.start,
            end: arms.last().unwrap().1.get_span().end,
        },
        Box::new(value),
        arms,
    ))
}

// expression_values(&mut Parser) -> Result<Ast, ParseError>
// Parses an expression.
fn expression_values(parser: &mut Parser) -> Result<Ast, ParseError> {
    if let Ok(withy) = call_optional!(with, parser) {
        Ok(withy)
    } else if let Ok(lambda) = call_optional!(lambda, parser) {
        Ok(lambda)
    } else if let Ok(list) = call_optional!(list, parser) {
        Ok(list)
    } else if let Ok(matchy) = call_optional!(matchy, parser) {
        Ok(matchy)
    } else {
        application(parser)
    }
}

// apply_op(&mut Parser) -> Result<Ast::Infix, ParseError>
// Gets the next infix application.
fn apply_op(parser: &mut Parser) -> Result<Ast, ParseError> {
    infixr_op!(parser, expression_values, Token::Dollar, Token::Unreachable)
}

// expression(&mut Parser) -> Result<Ast, ParseError>
// Parses expressions chained by ;.
fn expression(parser: &mut Parser) -> Result<Ast, ParseError> {
    infixr_op!(parser, apply_op, Token::Semicolon, Token::Unreachable)
}

// annotation(&mut Parser) -> Result<Ast, ParseError>
// Parses an annotation.
fn annotation(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let (annotation, span) = consume_save!(parser, Annotation, state, false, "");
    Ok(Ast::Annotation(span, annotation))
}

// assignment_raw(&mut Parser) -> Result<Ast, ParseError>
// Parses an assignment without any types or arguments.
fn assignment_raw(parser: &mut Parser) -> Result<Ast, ParseError> {
    // Get the variable name
    let state = parser.save_state();
    let (name, span) = consume_save!(parser, Symbol, state, false, "");

    // Get the assign operator
    consume_nosave!(parser, Assign, state, false, "");

    // Get the value
    newline(parser);
    let value = call_func_fatal!(expression, parser, "Expected value after `=`");

    Ok(Ast::Assign(
        Span {
            start: span.start,
            end: value.get_span().end,
        },
        name,
        Box::new(value),
    ))
}

// type_symbol(&mut Parser) -> Result<Ast, ParseError>
// Parses a type symbol or parenthesised type.
fn type_symbol(parser: &mut Parser) -> Result<Ast, ParseError> {
    let (token, span) = match parser.peek() {
        Some(v) => v,
        None => return ParseError::empty(),
    };

    // Symbols
    if let Token::Symbol = token {
        let value = Ast::Symbol(span, parser.slice());
        parser.next();
        Ok(value)

    // Generics
    } else if let Token::Generic(v) = token {
        let value = Ast::Generic(span, v.to_owned());
        parser.next();
        Ok(value)

    // Parenthesised types
    } else if let Token::LParen = token {
        // Get value
        let state = parser.save_state();
        parser.next();
        newline(parser);

        let value = match type_expr(parser) {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get right parenthesis
        newline(parser);
        consume_nosave!(parser, RParen, state, true, "Expected right parenthesis");
        Ok(value)

    // Not a value
    } else {
        ParseError::empty()
    }
}

// type_tagged(&mut Parser) -> Result<Ast< ParseError>
// Parses a tagged type (a: T).
fn type_tagged(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let s = call_func!(symbol, parser, state);
    consume_nosave!(parser, Colon, state, false, "");
    let t = call_func_fatal!(type_symbol, parser, "Expected type after `:`");

    Ok(Ast::Infix(
        Span {
            start: s.get_span().start,
            end: t.get_span().end,
        },
        String::from(":"),
        Box::new(s),
        Box::new(t),
    ))
}

// type_field(&mut Parser) -> Result<Ast, ParseError>
// Parses a field of a union or product type.
fn type_field(parser: &mut Parser) -> Result<Ast, ParseError> {
    if let Ok(v) = call_optional!(type_tagged, parser) {
        Ok(v)
    } else {
        type_symbol(parser)
    }
}

// type_union(&mut Parser) -> Result<Ast, ParseError>
// Parses a union type declaration.
fn type_union(parser: &mut Parser) -> Result<Ast, ParseError> {
    infixl_op!(parser, type_field, Token::Bar, Token::Unreachable)
}

// type_func(&mut Parser) -> Result<Ast, ParseError>
// Parses a function type.
fn type_func(parser: &mut Parser) -> Result<Ast, ParseError> {
    infixr_op!(parser, type_union, Token::RightArrow, Token::Unreachable)
}

// type_expr(&mut Parser) -> Result<Ast, ParseError>
// Parses a type.
fn type_expr(parser: &mut Parser) -> Result<Ast, ParseError> {
    infixl_op!(parser, type_func, Token::PlusArrow, Token::Unreachable)
}

// type_assignment(&mut Parser) -> Result<Ast, ParseError>
// Parses an assignment of a type.
fn type_assignment(parser: &mut Parser) -> Result<Ast, ParseError> {
    // Get type keyword
    let state = parser.save_state();
    let (_, span) = consume_save!(parser, Type, state, false, "");

    // Get name of type
    let (name, _) = consume_save!(parser, Symbol, state, true, "Expected symbol after type");

    // Get assignment operator
    consume_nosave!(parser, Assign, state, true, "Expected `=` after type name");
    newline(parser);

    // Get type
    let _type = call_func_fatal!(type_expr, parser, "Expected type after `=`");

    // Successfully return
    Ok(Ast::AssignType(
        Span {
            start: span.start,
            end: _type.get_span().end,
        },
        name,
        Box::new(_type),
    ))
}

// declaration(&mut Parser) -> Result<(Span, String, Ast), ParseError>
// Parses a declaration.
fn declaration(parser: &mut Parser) -> Result<(Span, String, Ast), ParseError> {
    // Get the variable name
    let state = parser.save_state();
    let (name, span) = consume_save!(parser, Symbol, state, false, "");

    // Get the colon
    consume_nosave!(parser, Colon, state, false, "");

    // Get the type
    let type_val = call_func_fatal!(type_expr, parser, "Expected type after `:`");

    Ok((span, name, type_val))
}

// assignment_func(&mut Parser) -> Result<Ast, ParseError>
// Parses an assignment for a function.
fn assignment_func(parser: &mut Parser) -> Result<Ast, ParseError> {
    // Get the variable name
    let state = parser.save_state();
    let mut args = vec![];
    let (name, span) = consume_save!(parser, Symbol, state, false, "");

    // Get arguments
    loop {
        // Get comma
        if !args.is_empty() {
            match parser.peek() {
                Some((Token::Comma, _)) => {
                    parser.next();
                }

                _ => break,
            }
        }

        let arg = match declaration(parser) {
            Ok(v) => (v.1, v.2),
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        args.push(arg);
    }

    // Check that there is at least one argument
    if args.is_empty() {
        parser.return_state(state);
        return ParseError::empty();
    }

    // Get the assign operator
    let slice = parser.slice();
    consume_nosave!(parser, Assign, state, true, "Expected `=`, got `{}`", slice);

    // Get the value
    newline(parser);
    let value = call_func_fatal!(expression, parser, "Expected function body after `=`");

    Ok(Ast::AssignFunction(
        Span {
            start: span.start,
            end: value.get_span().end,
        },
        name,
        args,
        Box::new(value),
    ))
}

// assignment(&mut Parser) -> Result<Ast, ParseError>
// Parses an assignment.
fn assignment(parser: &mut Parser) -> Result<Ast, ParseError> {
    if let Ok(typed) = call_optional!(assignment_raw, parser) {
        Ok(typed)
    } else {
        assignment_func(parser)
    }
}

// with(&mut Parser) -> Result<Ast, ParseError>
// Parses a with expression (scoping).
fn with(parser: &mut Parser) -> Result<Ast, ParseError> {
    // Get the with keyword
    let state = parser.save_state();
    let span = parser.span();
    consume_nosave!(parser, Let, state, false, "");

    // Get assignments
    let mut assigns = vec![];
    loop {
        let assign = match assignment(parser) {
            Ok(v) => v,
            Err(e) if e.fatal => return Err(e),
            Err(_) => break,
        };
        assigns.push(assign);

        // Newline
        newline(parser);
    }

    // Check that there is at least one assignment
    if assigns.is_empty() {
        parser.return_state(state);
        return ParseError::empty();
    }

    // Get the body
    consume_nosave!(parser, In, state, true, "Expected `in` after let bindings");
    newline(parser);
    let body = call_func!(expression, parser, state);

    Ok(Ast::With(
        Span {
            start: span.start,
            end: body.get_span().end,
        },
        assigns,
        Box::new(body),
    ))
}

/*
// import(&mut Parser) -> Result<Ast, ParseError>
// Parses an import statement.
fn import(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let (_, span) = consume_save!(parser, Import, state, false, "");
    let start = span.start;

    let name = call_func_fatal!(access_member, parser, "Expected module name after `import`");
    let mut end = name.get_span().end;

    let qualified = !matches!(
        parser.peek(),
        Some((Token::LParen, _))
    );

    if qualified {
        let mut alias = String::with_capacity(0);
        if let Some((Token::As, _)) = parser.peek() {
            parser.next();
            let (a, s) = consume_save!(parser, Symbol, state, true, "Expected alias after `as`");
            end = s.end;
            alias = a
        }

        Ok(Ast::QualifiedImport(
            Span { start, end },
            Box::new(name),
            alias,
        ))
    } else {
        let mut imports = vec![];
        parser.next();
        loop {
            newline(parser);
            if parser.peek().is_none() {
                parser.return_state(state);
                return Err(ParseError {
                    span: parser.span(),
                    msg: String::from(
                        "Expected imported item or right parenthesis, got end of file",
                    ),
                    fatal: true,
                });
            }

            if imports.is_empty() {
                if let Some((Token::Mul, _)) = parser.peek() {
                    parser.next();
                    consume_nosave!(parser, RParen, state, true, "Expected right parenthesis");
                    break;
                }
            } else {
                match parser.peek() {
                    Some((Token::Comma, _)) => {
                        parser.next();
                    },

                    Some((Token::RParen, _)) => {
                        parser.next();
                        break;
                    }

                    _ => {
                        return Err(ParseError {
                            span: parser.span(),
                            msg: String::from("Expected comma or right parenthesis"),
                            fatal: true,
                        });
                    }
                }
            }

            let (token, span) = parser.peek().unwrap();
            end = span.end;

            match token {
                Token::Symbol => imports.push(parser.slice()),
                _ => {
                    parser.return_state(state);
                    return Err(ParseError {
                        span: parser.span(),
                        msg: String::from("Expected imported item"),
                        fatal: true,
                    });
                }
            }

            parser.next();
        }

        parser.next();
        Ok(Ast::Import(Span { start, end }, Box::new(name), imports))
    }
}

// header(&mut Parser) -> Result<Ast, ParseError>
// Parses a header entry.
fn header(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    let (_, span) = consume_save!(parser, Module, state, false, "");
    let start = span.start;
    let name = call_func_fatal!(access_member, parser, "Expected module name after `module`");
    let mut end = name.get_span().end;

    let mut exports = vec![];
    newline(parser);
    let mut comma = false;
    if let Some((Token::LParen, _)) = parser.peek() {
        parser.next();

        loop {
            newline(parser);
            if comma {
                match parser.peek() {
                    Some((Token::Comma, _)) => {
                        parser.next();
                    }
                    Some((Token::RParen, _)) => break,
                    _ => (),
                }
            } else {
                comma = true;
            }

            newline(parser);
            if parser.peek().is_none() {
                let span = parser.span();
                parser.return_state(state);
                return Err(ParseError {
                    span,
                    msg: String::from(
                        "Expected exported item or right parenthesis, got end of file",
                    ),
                    fatal: true,
                });
            }

            let (token, span) = parser.peek().unwrap();
            end = span.end;

            match token {
                Token::RParen => break,
                Token::Symbol => exports.push(match declaration(parser) {
                    Ok(v) => v,
                    Err(e) => {
                        parser.return_state(state);
                        return Err(e);
                    }
                }),

                Token::Type => {
                    parser.next();
                    if let Some((Token::Symbol, _)) = parser.peek() {
                        exports.push((parser.span(), parser.slice(), Ast::Empty));
                        parser.next();
                    } else {
                        return Err(ParseError {
                            span: parser.span(),
                            msg: String::from("Expected type name after `type`"),
                            fatal: true,
                        });
                    }
                }

                _ => {
                    let span = parser.span();
                    parser.return_state(state);
                    return Err(ParseError {
                        span,
                        msg: String::from("Expected exported item or right parenthesis"),
                        fatal: true,
                    });
                }
            }
        }

        parser.next();
    }

    newline(parser);

    let mut imports = vec![];
    while let Ok(v) = call_optional!(import, parser) {
        imports.push(v);
        newline(parser);
    }

    Ok(Ast::Header(
        Span { start, end },
        Box::new(name),
        exports,
        imports,
    ))
}

// externy(&mut Parser) -> Result<Ast, ParseError>
// Parses an external function declaration.
fn externy(parser: &mut Parser) -> Result<Ast, ParseError> {
    let state = parser.save_state();
    consume_nosave!(parser, Extern, state, false, "");

    let (c_func, s) = if let Some((Token::String(s), v)) = parser.peek() {
        (s.clone(), v)
    } else {
        return Err(ParseError {
            span: parser.span(),
            msg: String::from("Expected string literal after `extern`"),
            fatal: true,
        });
    };

    parser.next();
    newline(parser);
    let (name, _) = consume_save!(
        parser,
        Symbol,
        state,
        true,
        "Expected symbol after external function declaration"
    );
    consume_nosave!(
        parser,
        Colon,
        state,
        true,
        "Expected `:` after foreign function declaration"
    );
    let _type = call_func_fatal!(type_expr, parser, "Expected type after `:`");

    Ok(Ast::Extern(
        Span {
            start: s.start,
            end: _type.get_span().end,
        },
        c_func,
        name,
        Box::new(_type),
    ))
}
*/

// parse(&str) -> Result<Ast, ParseError>
// Parses curly code.
pub fn parse(s: &str) -> Result<Vec<Ast>, ParseError> {
    let mut parser = Parser::new(s);
    let mut lines = vec![];
    let p = &mut parser;

    newline(p);
    /*
    if let Ok(header) = call_optional!(header, p) {
        lines.push(header);
    }*/

    while p.peek().is_some() {
        // Parse one line
        if let Ok(annotation) = call_optional!(annotation, p) {
            lines.push(annotation);
        } else if let Ok(assign) = call_optional!(assignment, p) {
            lines.push(assign);
        } else {
            lines.push(match type_assignment(p) {
                Ok(v) => v,
                Err(e) if e.fatal => return Err(e),
                Err(_) => {
                    let peeked = if p.peek().is_some() {
                        p.slice()
                    } else {
                        String::from("eof")
                    };
                    return Err(ParseError {
                        span: p.span(),
                        msg: format!("Unexpected `{}`", peeked),
                        fatal: true,
                    });
                }
            });
        }
        /*
        } else if let Ok(_type) = call_optional!(type_assignment, p) {
            lines.push(_type);
        } else {
            lines.push(match externy(p) {
                Ok(v) => v,
                Err(e) if e.fatal => return Err(e),
                Err(_) => {
                    let peeked = if p.peek().is_some() {
                        p.slice()
                    } else {
                        String::from("eof")
                    };
                    return Err(ParseError {
                        span: p.span(),
                        msg: format!("Unexpected `{}`", peeked),
                        fatal: true,
                    });
                }
            });
            */

        // Skip newlines
        newline(p);
    }

    Ok(lines)
}
