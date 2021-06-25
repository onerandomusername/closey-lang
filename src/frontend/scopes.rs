use std::collections::HashMap;

use super::ir::Location;
use super::types::TypeRc;

#[derive(Debug, Default)]
pub struct Scope {
    pub variables: HashMap<String, (TypeRc, usize, Location, bool, String)>,
    pub parent: Option<Box<Scope>>,
    new_func: bool,
}

impl Scope {
    // new() -> Scope
    // Creates a new empty scope.
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::with_capacity(0),
            parent: None,
            new_func: false,
        }
    }

    // put_var_raw(&mut self, String, TypeRc, usize, Span, bool) -> ()
    // Puts a variable in the current scope.
    pub fn put_var_raw(
        &mut self,
        name: String,
        _type: TypeRc,
        arity: usize,
        loc: Location,
        assigned: bool,
        origin: String,
    ) {
        self.variables
            .insert(name, (_type, arity, loc, assigned, origin));
    }

    // put_var(&mut self, &str, usize, Span, bool) -> ()
    // Puts a variable in the current scope.
    pub fn put_var(
        &mut self,
        name: &str,
        _type: &TypeRc,
        arity: usize,
        loc: &Location,
        assigned: bool,
        origin: &str,
    ) {
        self.variables.insert(
            String::from(name),
            (
                _type.clone(),
                arity,
                loc.clone(),
                assigned,
                String::from(origin),
            ),
        );
    }

    // get_var(&self, &str) -> Option<&(Type, usize, Location, bool, String)>
    // Gets a variable from the stack of scopes.
    pub fn get_var(&self, name: &str) -> Option<&(TypeRc, usize, Location, bool, String)> {
        // Set up
        let name = String::from(name);
        let mut scope = self;

        loop {
            // Return success if found
            if let Some(v) = scope.variables.get(&name) {
                return Some(v);
            }

            // Get next scope
            scope = match &scope.parent {
                Some(v) => &**v,
                None => break None,
            }
        }
    }

    // push_scope(&mut self, bool) -> ()
    // Pushes a new scope to the top of the scope stack.
    pub fn push_scope(&mut self, new_func: bool) {
        use std::mem::swap;

        let mut scope = Scope::new();
        scope.new_func = new_func;

        swap(&mut scope, self);
        self.parent = Some(Box::new(scope));
    }

    // pop_scop(&mut self) -> ()
    // Pops a scope from the stack if a parent scope exists.
    pub fn pop_scope(&mut self) {
        use std::mem::swap;

        if let Some(v) = &mut self.parent {
            let mut scope = Scope::new();

            swap(&mut scope, v);
            swap(self, &mut scope);
        }
    }

    // is_captured(&self, &str) -> bool
    // Returns true if captured from a new function
    pub fn is_captured(&self, name: &str) -> bool {
        // Set up
        let mut scope = self;
        let mut last_new_func = false;
        let mut new_func;

        loop {
            // Global scope is not captured
            if scope.parent.is_none() {
                break false;
            }

            // Update new_func if in a new function
            new_func = last_new_func;
            if scope.new_func {
                last_new_func = true;
            }

            // Return success if found
            if scope.variables.get(name).is_some() {
                break new_func;
            }

            // Get next scope
            scope = match &scope.parent {
                Some(v) => &**v,
                None => break false,
            }
        }
    }
}
