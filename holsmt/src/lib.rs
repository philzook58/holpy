mod ast;

use ast::{error, pool::TermPool, rc::Rc, Type};
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyTuple};
use std::cell::RefCell;
use std::thread_local;

// thread_local! creates a thread-local variable that supports Rc.
// RefCell supports interior mutability.
thread_local! {
    static CACHE: RefCell<TermPool> = RefCell::new(TermPool::new());
}

fn push_cache(key: &Type) -> ast::rc::Rc<Type> {
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some(value) = cache.types.get(&key) {
            return ast::rc::Rc::clone(value);
        }
        let value = ast::rc::Rc::new(key.clone());
        cache.types.insert(key.clone(), ast::rc::Rc::clone(&value));
        value
    })
}

// todo use unsendable | std::sync::Arc
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[pyclass(unsendable, name = "Type", subclass)]
struct PyType {
    ptr: ast::rc::Rc<ast::Type>,
}

#[pyclass(subclass, name = "TVar", extends = PyType)]
struct PyTVar;

#[pymethods]
impl PyTVar {
    #[new]
    fn new(name: String) -> (Self, PyType) {
        let ty = Type::new_TVar(name);
        let ty_ref = push_cache(&ty);

        (PyTVar {}, PyType { ptr: ty_ref })
    }
}

#[pyclass(subclass, name = "STVar", extends = PyType)]
struct PySTVar;

#[pymethods]
impl PySTVar {
    #[new]
    fn new(name: String) -> (Self, PyType) {
        let stvar = Type::new_STVar(name);
        let stvar_ref = push_cache(&stvar);

        (PySTVar {}, PyType { ptr: stvar_ref })
    }
}

#[pyclass(subclass, name = "TConst", extends = PyType)]
struct PyTConst;

#[pymethods]
impl PyTConst {
    #[new]
    #[pyo3(signature = (name, *args))]
    fn new(name: String, args: &PyTuple) -> (Self, PyType) {
        let mut type_args = Vec::new();

        for arg in args {
            if let Ok(pytype) = arg.extract::<PyType>() {
                type_args.push(ast::rc::Rc::clone(&pytype.ptr))
            } else {
                // process error
                unimplemented!()
            }
        }

        let tconst = Type::new_TConst(name, type_args);
        let tconst_ref = push_cache(&tconst);

        (PyTConst {}, PyType { ptr: tconst_ref })
    }
}

// PyResult<T> is an alias for Result<T, PyErr>
#[pymethods]
impl PyType {
    // todo, paser string, remove this method?
    #[new]
    fn new(name: String, args: Option<String>) -> Self {
        Self {
            ptr: ast::rc::Rc::new(Type::new_TVar(name)),
        }
    }

    fn is_tvar(&self) -> PyResult<bool> {
        Ok(self.ptr.is_tvar())
    }

    fn is_tconst(&self) -> PyResult<bool> {
        Ok(self.ptr.is_tconst())
    }

    fn is_fun(&self) -> PyResult<bool> {
        Ok(self.ptr.is_fun())
    }

    fn is_stvar(&self) -> PyResult<bool> {
        Ok(self.ptr.is_stvar())
    }

    fn domain_type(&self) -> PyResult<PyType> {
        match self.ptr.domain_type() {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(error.into()),
        }
        // Ok(self.ptr.domain_type().unwrap())
    }

    fn range_type(&self) -> PyResult<PyType> {
        match self.ptr.range_type() {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(error.into()),
        }
        // Ok(self.ptr.range_type().unwrap())
    }

    fn strip_type(&self) -> PyResult<(Vec<PyType>, PyType)> {
        match self.ptr.strip_type() {
            Ok((domains, range)) => Ok((
                domains
                    .iter()
                    .map(|domain| PyType {
                        ptr: ast::rc::Rc::clone(domain),
                    })
                    .collect(),
                PyType {
                    ptr: ast::rc::Rc::clone(&range),
                },
            )),
            Err(error) => Err(error.into()),
        }
    }

    fn size(&self) -> PyResult<usize> {
        // todo: process error
        Ok(self.ptr.size().unwrap())
    }

    /// There are some magic methods.
    fn __str__(&self) -> String {
        format!("{}", self.ptr)
    }

    fn __repr__(&self) -> String {
        self.ptr.repr()
    }
}

#[pyfunction(signature = (*args))]
fn TFun(args: &PyTuple) -> PyType {
    let mut type_args = Vec::new();

    for arg in args {
        if let Ok(pytype) = arg.extract::<PyType>() {
            type_args.push(ast::rc::Rc::clone(&pytype.ptr))
        } else {
            // process error
            unimplemented!()
        }
    }

    let mut res = type_args.pop().unwrap();
    for arg in type_args.iter().rev() {
        let name = "fun".to_string();
        let tconst = Type::new_TConst(name, vec![ast::rc::Rc::clone(arg), res]);
        res = push_cache(&tconst);
    }

    PyType { ptr: res }
}

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

// debug, print Rc<Type> memory address
#[pyfunction]
fn print_address(pytype: PyType) -> PyResult<String> {
    let raw_ptr: *const ast::Type = &*pytype.ptr;
    let raw_pytype: *const PyType = &pytype;
    Ok(format!(
        "pytype addr: {:?}, type addr: {:?}, strong cout: {}",
        raw_pytype,
        raw_ptr,
        ast::rc::Rc::strong_count(&pytype.ptr)
    ))
}

/// A Python module implemented in Rust.
#[pymodule]
fn holrs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sum_as_string, m)?)?;
    m.add_function(wrap_pyfunction!(print_address, m)?)?;
    m.add_function(wrap_pyfunction!(TFun, m)?)?;
    m.add_class::<PyType>()?;
    m.add_class::<PyTVar>()?;
    m.add_class::<PySTVar>()?;
    m.add_class::<PyTConst>()?;
    Ok(())
}
