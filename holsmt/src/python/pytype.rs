use crate::ast;
use crate::ast::{pool::TermPool, rc::Rc, term::Term, Type};
use crate::python::error::{
    CheckProofException, InvalidDerivationException, ParameterQueryException, TacticException,
    TermException, TheoryException, TypeCheckException, TypeException, TypeMatchException,
};
use crate::python::pyterm::{register_term_module, Abs, Bound, Comb, Const, PyTerm, SVar, Var};
use crate::{push_term_to_cache, push_type_to_cache};
use pyo3::pyclass::CompareOp;
use pyo3::types::{PyDict, PyTuple};
use pyo3::{prelude::*, PyNativeType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::thread_local;

#[pyclass(unsendable, subclass)]
#[derive(Default)]
pub struct TyInst {
    pub data: HashMap<String, PyType>,
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

    pub fn __str__(&self) -> String {
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
pub struct PyType {
    pub ptr: ast::rc::Rc<ast::Type>,
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
pub fn STVar(name: String) -> PyType {
    let stvar = Type::new_STVar(name);
    let stvar_ref = push_type_to_cache(stvar);

    PyType { ptr: stvar_ref }
}

// todo
#[pyfunction]
pub fn TVar(name: String) -> PyType {
    let tvar = Type::new_TVar(name);
    let tvar_ref = push_type_to_cache(tvar);

    PyType { ptr: tvar_ref }
}

// todo
#[pyfunction(signature = (name, *args))]
pub fn TConst(name: String, args: &PyTuple) -> PyType {
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
pub fn TFun(args: &PyTuple) -> PyType {
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
pub fn print_address(pytype: PyType) -> PyResult<String> {
    let raw_ptr: *const ast::Type = &*pytype.ptr;
    let raw_pytype: *const PyType = &pytype;
    Ok(format!(
        "pytype addr: {:?}, type addr: {:?}, strong cout: {}",
        raw_pytype,
        raw_ptr,
        ast::rc::Rc::strong_count(&pytype.ptr)
    ))
}
