mod ast;

use ast::{error, pool::TermPool, rc::Rc, term::Term, Type};
use pyo3::create_exception;
use pyo3::exceptions::PyException;
use pyo3::pyclass::CompareOp;
use pyo3::types::{PyDict, PyIterator, PyList, PyTuple};
use pyo3::{prelude::*, PyNativeType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::thread_local;

// create TypeMatchException
create_exception!(holrs, TypeMatchException, PyException);

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

fn push_type_to_cache(key: Type) -> ast::rc::Rc<Type> {
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.add_type(key)
    })
}

fn push_term_to_cache(key: Term) -> ast::rc::Rc<Term> {
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.add_term(key)
    })
}

#[pyclass(unsendable, subclass)]
#[derive(Default)]
struct TyInst {
    data: HashMap<String, PyType>,
}

impl ToPyObject for TyInst {
    fn to_object(&self, py: Python) -> PyObject {
        self.data.to_object(py)
    }
}

#[pymethods]
impl TyInst {
    #[new]
    #[pyo3(signature = (*dict, **kwargs))]
    fn new(dict: &PyTuple, kwargs: Option<&PyDict>) -> Self {
        let mut data = HashMap::new();

        if !dict.is_empty() {
            let _dict = dict.get_item(0).unwrap().downcast::<PyDict>().unwrap();
            for (key, value) in _dict.iter() {
                if let Ok(key_str) = key.extract::<String>() {
                    if let Ok(value_type) = value.extract::<PyType>() {
                        data.insert(key_str, value_type);
                    }
                }
            }
            Self { data }
        } else if let Some(kwargs_dict) = kwargs {
            for (key, value) in kwargs_dict.iter() {
                if let Ok(key_str) = key.extract::<String>() {
                    if let Ok(value_type) = value.extract::<PyType>() {
                        data.insert(key_str, value_type);
                    }
                }
            }
            Self { data }
        } else {
            panic!("Invalid argument")
        }
    }

    fn __str__(&self) -> String {
        // self.data
        //     .iter()
        //     .map(|(name, T)| format!("{}: {}", name, T.ptr))
        //     .collect::<Vec<_>>()
        //     .join(", ")
        let mut result = String::new();
        for (name, ty) in &self.data {
            result.push_str(&format!("{} := {}, ", name, ty.ptr));
        }
        // Remove trailing comma and space
        if let Some(last) = result.rfind(", ") {
            result.truncate(last);
        }
        result
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> bool {
        match op {
            CompareOp::Eq => self.data == other.data,
            CompareOp::Ne => self.data != other.data,
            _ => self.data == other.data,
        }
    }
}

// todo use unsendable | std::sync::Arc
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[pyclass(unsendable, name = "Type", subclass)]
struct PyType {
    ptr: ast::rc::Rc<ast::Type>,
}

impl ToPyObject for PyType {
    fn to_object(&self, py: Python) -> PyObject {
        // todo, clone()?
        self.clone().into_py(py)
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

    fn is_tvar(&self) -> bool {
        self.ptr.is_tvar()
    }

    fn is_tconst(&self) -> bool {
        self.ptr.is_tconst()
    }

    fn is_fun(&self) -> bool {
        self.ptr.is_fun()
    }

    fn is_stvar(&self) -> bool {
        self.ptr.is_stvar()
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

    fn get_stvars(&self) -> PyResult<Vec<PyType>> {
        let res = self.ptr.get_stvars();
        Ok(res
            .iter()
            .map(|stvar| PyType {
                ptr: ast::rc::Rc::clone(stvar),
            })
            .collect())
    }

    fn get_tvars(&self) -> PyResult<Vec<PyType>> {
        let res = self.ptr.get_tvars();
        Ok(res
            .iter()
            .map(|tvar| PyType {
                ptr: ast::rc::Rc::clone(tvar),
            })
            .collect())
    }

    fn get_tsubs(&self) -> PyResult<Vec<PyType>> {
        let res = self.ptr.get_tsubs();
        Ok(res
            .iter()
            .map(|tsub| PyType {
                ptr: ast::rc::Rc::clone(tsub),
            })
            .collect())
    }

    #[pyo3(signature = (tyinst=None, **kwargs))]
    fn subst(&self, tyinst: Option<&TyInst>, kwargs: Option<&PyDict>) -> PyResult<PyType> {
        // Create a HashMap to hold the type instantiation
        let mut subst_tyinst: HashMap<String, Rc<Type>> = HashMap::new();

        if let Some(tyinst_dict) = tyinst {
            // Iterate over the items in `tyinst_dict` and populate the HashMap
            for (key, value) in tyinst_dict.data.iter() {
                subst_tyinst.insert(key.clone(), ast::rc::Rc::clone(&value.ptr));
            }
        } else if let Some(kwargs_dict) = kwargs {
            // If `tyinst` is not provided, try extracting the type instantiation from `kwargs`
            for (key, value) in kwargs_dict.iter() {
                if let Ok(key_str) = key.extract::<String>() {
                    if let Ok(value_type) = value.extract::<PyType>() {
                        // todo, rc::Rc::clone()
                        // subst_tyinst.insert(key_str, value_type.ptr);
                        subst_tyinst.insert(key_str, ast::rc::Rc::clone(&value_type.ptr));
                    }
                }
            }
        }

        match self.ptr.subst(&subst_tyinst, &push_type_to_cache) {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(error.into()),
        }
    }

    // todo match is a Rust keyword, so we use _match instead.

    fn _match(&self, T: PyType, py: Python) -> PyResult<TyInst> {
        match self.ptr._match(&T.ptr) {
            Ok(res) => {
                let hashmap: HashMap<String, PyType> = res
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.clone(),
                            PyType {
                                ptr: ast::rc::Rc::clone(value),
                            },
                        )
                    })
                    .collect();

                let tyinst = TyInst { data: hashmap };
                // Ok(tyinst)
                Ok(tyinst)
            }
            Err(error) => Err(TypeMatchException::new_err(error.to_string())),
        }
    }

    fn test_type_convert(&self) -> HashMap<String, String> {
        let mut res = HashMap::new();
        res.insert("a".to_string(), "b".to_string());
        res
    }

    fn is_numeral_type(&self) -> PyResult<bool> {
        Ok(self.ptr.is_numeral_type())
    }

    /// There are some magic methods.
    fn __str__(&self) -> String {
        format!("{}", self.ptr)
    }

    fn __repr__(&self) -> String {
        self.ptr.repr()
    }

    /// There are some magic methods. 0.19.3
    fn __richcmp__(&self, other: &Self, op: CompareOp) -> bool {
        match op {
            CompareOp::Eq => self.ptr == other.ptr,
            CompareOp::Ne => self.ptr != other.ptr,
            _ => self.ptr == other.ptr,
        }
    }
}

#[pyfunction]
fn STVar(name: String) -> PyType {
    let stvar = Type::new_STVar(name);
    let stvar_ref = push_type_to_cache(stvar);

    PyType { ptr: stvar_ref }
}

// todo
#[pyfunction]
fn TVar(name: String) -> PyType {
    let tvar = Type::new_TVar(name);
    let tvar_ref = push_type_to_cache(tvar);

    PyType { ptr: tvar_ref }
}

// todo
#[pyfunction(signature = (name, *args))]
fn TConst(name: String, args: &PyTuple) -> PyType {
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
    let tconst_ref = push_type_to_cache(tconst);

    PyType { ptr: tconst_ref }
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
        res = push_type_to_cache(tconst);
    }

    PyType { ptr: res }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[pyclass(unsendable, name = "Term", subclass)]
struct PyTerm {
    ptr: ast::rc::Rc<Term>,
}

#[pymethods]
impl PyTerm {
    // todo, unneccessary?
    #[new]
    fn new(name: String, ty: PyType) -> Self {
        Self {
            ptr: ast::rc::Rc::new(Term::new_var(name, ty.ptr)),
        }
    }

    /// There are some magic methods.
    fn __str__(&self) -> String {
        format!("{}", self.ptr)
    }

    fn __repr__(&self) -> String {
        self.ptr.repr()
    }

    #[pyo3(signature = (*args))]
    fn __call__(&self, args: &PyTuple) -> Self {
        let mut term_args = Vec::new();

        for arg in args {
            if let Ok(pyterm) = arg.extract::<PyTerm>() {
                term_args.push(ast::rc::Rc::clone(&pyterm.ptr))
            } else {
                // process error
                unimplemented!()
            }
        }

        // args always in CACHE
        let mut res = ast::rc::Rc::clone(&self.ptr);
        for arg in term_args {
            res = push_term_to_cache(Term::new_comb(res, arg));
        }
        PyTerm { ptr: res }
    }

    /// There are some magic methods. 0.19.3
    fn __richcmp__(&self, other: &Self, op: CompareOp) -> bool {
        match op {
            CompareOp::Eq => {
                if self.ptr == other.ptr {
                    return true;
                }

                self.ptr.eq(&other.ptr)
            }
            CompareOp::Ne => {
                if self.ptr != other.ptr {
                    return true;
                }

                !self.ptr.eq(&other.ptr)
            }
            _ => self.ptr == other.ptr,
        }
    }

    fn get_type(&self) -> PyResult<PyType> {
        // funciton ptr
        match self
            .ptr
            .get_type(&(push_type_to_cache as fn(Type) -> ast::rc::Rc<Type>))
        {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(error.into()),
        }
    }

    fn is_open(&self) -> bool {
        self.ptr.is_open()
    }

    #[pyo3(signature = (tyinst=None, **kwargs))]
    fn subst_type(&self, tyinst: Option<&TyInst>, kwargs: Option<&PyDict>) -> PyTerm {
        let subst_dict: HashMap<String, Rc<Type>> = match tyinst {
            Some(dict) => dict
                .data
                .iter()
                .map(|(k, v)| (k.clone(), v.ptr.clone()))
                .collect(),
            None => {
                let mut subst_dict = HashMap::new();
                if let Some(kwargs_dict) = kwargs {
                    for (key, value) in kwargs_dict.iter() {
                        if let Ok(key_str) = key.extract::<String>() {
                            if let Ok(value_type) = value.extract::<PyType>() {
                                subst_dict.insert(key_str, ast::rc::Rc::clone(&value_type.ptr));
                            }
                        }
                    }
                }
                subst_dict
            }
        };

        PyTerm {
            ptr: self.ptr.subst_type(
                &subst_dict,
                &(push_term_to_cache as fn(Term) -> ast::rc::Rc<Term>),
                &(push_type_to_cache as fn(Type) -> ast::rc::Rc<Type>),
            ),
        }
    }

    fn is_conj(&self) -> bool {
        self.ptr.is_conj()
    }

    fn is_disj(&self) -> bool {
        self.ptr.is_disj()
    }

    fn is_forall(&self) -> bool {
        self.ptr.is_forall()
    }

    fn is_exists(&self) -> bool {
        self.ptr.is_exists()
    }

    fn is_let(&self) -> bool {
        self.ptr.is_let()
    }

    fn is_equal(&self) -> bool {
        self.ptr.is_equal()
    }

    fn is_VAR(&self) -> bool {
        self.ptr.is_var()
    }
}

#[pyfunction]
fn SVar(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_svar(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction]
fn Var(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_var(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction]
fn Const(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_const(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

// todo, Rc::clone()?
#[pyfunction]
fn Comb(fun: PyTerm, arg: PyTerm) -> PyTerm {
    let term = Term::new_comb(fun.ptr, arg.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction(signature = (*args))]
fn Abs(args: &PyTuple) -> PyTerm {
    if args.len() < 3 || args.len() % 2 == 0 {
        panic!("Invalid argument length");
    }
    let args_vec: Vec<&PyAny> = args.iter().collect();
    let mut args_iter = args_vec.iter().rev();
    let body = args_iter
        .next()
        .and_then(|arg| arg.extract::<PyTerm>().ok())
        .expect("Invalid argument: body");
    let mut current_term = ast::rc::Rc::clone(&body.ptr);

    loop {
        let var_ty = match args_iter
            .next()
            .and_then(|arg| arg.extract::<PyType>().ok())
        {
            Some(var_ty) => var_ty,
            None => break,
        };
        let var_name = match args_iter
            .next()
            .and_then(|arg| arg.extract::<String>().ok())
        {
            Some(var_name) => var_name,
            None => break,
        };

        let new_term = Term::Abs(var_name, var_ty.ptr, current_term);
        current_term = push_term_to_cache(new_term);
    }

    // println!("{:?}", current_term);
    PyTerm { ptr: current_term }
}

#[pyfunction]
fn Bound(n: usize) -> PyTerm {
    let term = Term::new_bound(n);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

/// A Python module implemented in Rust.
#[pymodule]
#[pyo3(name = "holrs")] // todo, modify name
fn holrs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(print_address, m)?)?;
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

    m.add_class::<PyType>()?;
    m.add_class::<PyTerm>()?;
    m.add_class::<TyInst>()?;

    // add exception
    m.add("TypeMatchException", _py.get_type::<TypeMatchException>())?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use pyo3::prelude::*;
    use pyo3::py_run;
    use pyo3::types::PyList;

    #[test]
    fn test_strip_type() {
        Python::with_gil(|py| {
            let list = PyList::new(py, &[1, 2, 3]);
            py_run!(py, list, "print(list)");
        });

        println!("testStripType");
    }
}
