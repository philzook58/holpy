pub mod ast;
mod parser;
pub mod python;

use crate::python::pyterm::{
    register_term_module, Abs, Binary, Bound, Comb, Const, Inst, PyTerm, SVar, Var,
};
use crate::python::pytype::{print_address, PyType, STVar, TConst, TFun, TVar, TyInst};
use ast::{pool::TermPool, term::Term, Type};
use pyo3::prelude::*;
use python::error::{
    CheckProofException, InvalidDerivationException, ParameterQueryException, TacticException,
    TermException, TheoryException, TypeCheckException, TypeException, TypeMatchException,
};
use std::cell::RefCell;
use std::thread_local;

// thread_local! creates a thread-local variable that supports Rc.
// RefCell supports interior mutability.
thread_local! {
    static CACHE: RefCell<TermPool> = RefCell::new(TermPool::new());
}

// print CACHE
#[pyfunction]
fn print_cache_types() {
    CACHE.with(|cache| {
        println!("{:#?}", cache.borrow());
    })
}

pub(crate) fn push_type_to_cache(key: Type) -> ast::rc::Rc<Type> {
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.add_type(key)
    })
}

pub(crate) fn push_term_to_cache(key: Term) -> ast::rc::Rc<Term> {
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.add_term(key)
    })
}

/// A Python module implemented in Rust.
#[pymodule]
#[pyo3(name = "holrs")] // todo, modify name
fn holrs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(print_address, m)?)?;
    m.add_function(wrap_pyfunction!(print_cache_types, m)?)?;
    m.add_function(wrap_pyfunction!(TFun, m)?)?;
    m.add_function(wrap_pyfunction!(STVar, m)?)?;
    m.add_function(wrap_pyfunction!(TVar, m)?)?;
    m.add_function(wrap_pyfunction!(TConst, m)?)?;

    m.add_function(wrap_pyfunction!(SVar, m)?)?;
    m.add_function(wrap_pyfunction!(Var, m)?)?;
    m.add_function(wrap_pyfunction!(Const, m)?)?;
    m.add_function(wrap_pyfunction!(Comb, m)?)?;
    m.add_function(wrap_pyfunction!(Abs, m)?)?;
    m.add_function(wrap_pyfunction!(Bound, m)?)?;
    m.add_function(wrap_pyfunction!(Binary, m)?)?;

    m.add_class::<PyType>()?;
    m.add_class::<PyTerm>()?;
    m.add_class::<TyInst>()?;
    m.add_class::<Inst>()?;

    // add exception
    m.add("TypeException", _py.get_type::<TypeException>())?;
    m.add("TypeMatchException", _py.get_type::<TypeMatchException>())?;
    m.add("TypeCheckException", _py.get_type::<TypeCheckException>())?;

    m.add("TermException", _py.get_type::<TermException>())?;

    m.add(
        "InvalidDerivationException",
        _py.get_type::<InvalidDerivationException>(),
    )?;

    m.add("TheoryException", _py.get_type::<TheoryException>())?;
    m.add("CheckProofException", _py.get_type::<CheckProofException>())?;
    m.add(
        "ParameterQueryException",
        _py.get_type::<ParameterQueryException>(),
    )?;

    m.add("TacticException", _py.get_type::<TacticException>())?;

    let term_module = register_term_module(_py)?;
    m.add_submodule(term_module)?;

    Ok(())
}
