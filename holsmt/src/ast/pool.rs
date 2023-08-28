//! This module implements `TermPool`, a structure that stores terms and implements hash consing.

use super::{Rc, Term, Type};
use ahash::AHashMap;
use std::fmt;

/// A structure to store and manage all allocated types and terms.
pub struct TermPool {
    /// A map of the types in the pool.
    pub(crate) types: AHashMap<Type, Rc<Type>>,
    /// A map of the terms in the pool.
    pub(crate) terms: AHashMap<Term, Rc<Term>>,
}

impl Default for TermPool {
    fn default() -> Self {
        Self::new()
    }
}

impl TermPool {
    /// Constructs a new `TypePool`.'
    pub fn new() -> Self {
        Self {
            types: AHashMap::new(),
            terms: AHashMap::new(),
        }
    }

    fn add_type_to_map(types_map: &mut AHashMap<Type, Rc<Type>>, ty: Type) -> Rc<Type> {
        use std::collections::hash_map::Entry;

        match types_map.entry(ty) {
            Entry::Occupied(occupied_entry) => Rc::clone(occupied_entry.get()),
            Entry::Vacant(vacant_entry) => {
                let ty = vacant_entry.key().clone();
                vacant_entry.insert(Rc::new(ty)).clone()
            }
        }
    }

    /// Takes a type and returns a possibly newly allocated `Rc` that references it.
    pub fn add_type(&mut self, ty: Type) -> Rc<Type> {
        let ty = Self::add_type_to_map(&mut self.types, ty);
        // deduciton type, the store in cache?
        ty
    }

    pub fn add_all_types(&mut self, tys: Vec<Type>) -> Vec<Rc<Type>> {
        tys.into_iter().map(|t| self.add_type(t)).collect()
    }

    fn add_term_to_map(types_map: &mut AHashMap<Term, Rc<Term>>, ty: Term) -> Rc<Term> {
        use std::collections::hash_map::Entry;

        match types_map.entry(ty) {
            Entry::Occupied(occupied_entry) => Rc::clone(occupied_entry.get()),
            Entry::Vacant(vacant_entry) => {
                let ty = vacant_entry.key().clone();
                vacant_entry.insert(Rc::new(ty)).clone()
            }
        }
    }

    /// Takes a type and returns a possibly newly allocated `Rc` that references it.
    ///
    pub fn add_term(&mut self, ty: Term) -> Rc<Term> {
        let ty = Self::add_term_to_map(&mut self.terms, ty);
        // deduciton type, the store in cache?
        ty
    }

    pub fn add_all_terms(&mut self, tys: Vec<Term>) -> Vec<Rc<Term>> {
        tys.into_iter().map(|t| self.add_term(t)).collect()
    }
}

impl fmt::Debug for TermPool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "TermPool {{")?;

        for (key, value) in self.types.iter() {
            writeln!(
                f,
                "    {}: {{ value: {:?}, count: {}, ptr: {:p} }},",
                key,
                value,
                Rc::strong_count(value),
                Rc::as_ptr(value)
            )?;
        }

        write!(f, "}}")
    }
}

impl fmt::Display for TermPool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TermPool")
    }
}
