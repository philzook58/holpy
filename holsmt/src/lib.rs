mod ast;

use ast::{error, pool::TermPool, rc::Rc, Type};
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

#[pyclass(unsendable, subclass, extends=PyDict)]
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
    // *args, **kwargs support implicitly call __new__ method for BaseClass, when create a instance from Python
    // todo implementation by self use **kwargs? HashMap<String, PyType>
    #[new]
    #[pyo3(signature = (*args, **kwargs))]
    fn new(args: &PyAny, kwargs: Option<&PyAny>) -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    // fn __str__(&self) -> String {
    //     let gil = Python::acquire_gil();
    //     let py = gil.python();

    //     let dict: &PyDict = self.dict.as_ref(py);
    //     let items = dict.items(py);

    //     let mut result = Vec::new();

    //     for item in items.iter() {
    //         let (key, value) = item.extract::<(String, String)>(py).unwrap();
    //         result.push(format!("'{} := {}'", key, value));
    //     }

    //     result.join(", ")
    // }

    // fn __str__(mut self_: PyRefMut<'_, Self>) -> String {
    //     let py = self_.py();
    //     let dict: &PyDict = unsafe { py.from_borrowed_ptr_or_err(self_.as_ptr()).unwrap() };
    //     println!("{}", dict.len());
    //     // for (key, value) in self.iter() {
    //     //     println!("key: {}, value: {}", key, value);
    //     // }
    //     "".to_string()
    // }

    // fn set(mut self_: PyRefMut<'_, Self>, key: String, value: PyType) {
    //     let py = self_.py();
    //     let dict: &PyDict = unsafe { py.from_borrowed_ptr_or_err(self_.as_ptr()).unwrap() };
    //     dict.set_item(key, value).unwrap();
    // }
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
    fn subst(&self, tyinst: Option<&PyDict>, kwargs: Option<&PyDict>) -> PyResult<PyType> {
        // Create a HashMap to hold the type instantiation
        let mut subst_tyinst: HashMap<String, Rc<Type>> = HashMap::new();

        if let Some(tyinst_dict) = tyinst {
            // Iterate over the items in `tyinst_dict` and populate the HashMap
            for (key, value) in tyinst_dict.iter() {
                if let Ok(key_str) = key.extract::<String>() {
                    if let Ok(value_type) = value.extract::<PyType>() {
                        // todo, rc::Rc::clone()
                        // subst_tyinst.insert(key_str, value_type.ptr);
                        subst_tyinst.insert(key_str, ast::rc::Rc::clone(&value_type.ptr));
                    }
                }
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

        match self.ptr.subst(&subst_tyinst, &push_cache) {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(error.into()),
        }
    }

    // todo match is a Rust keyword, so we use _match instead.

    fn _match(&self, T: PyType, py: Python) -> PyResult<PyObject> {
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
                // Ok(mydict)
                Ok(tyinst.to_object(py))
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
#[pyo3(name = "holrs")] // todo, modify name
fn holrs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(print_address, m)?)?;
    m.add_function(wrap_pyfunction!(TFun, m)?)?;
    m.add_class::<PyType>()?;
    m.add_class::<PyTVar>()?;
    m.add_class::<PySTVar>()?;
    m.add_class::<PyTConst>()?;
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
