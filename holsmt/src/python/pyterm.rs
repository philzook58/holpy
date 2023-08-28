use crate::ast;
use crate::ast::{rc::Rc, term::Term, Type};
use crate::{
    push_term_to_cache, push_type_to_cache,
    python::pytype::{PyType, TyInst},
};
use pyo3::exceptions::PyAttributeError;
use pyo3::prelude::*;
use pyo3::pyclass::CompareOp;
use pyo3::types::{PyDateAccess, PyDict, PyList, PyTuple};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use super::error::HolPyErr;

#[pyclass(unsendable, subclass)]
#[derive(Default, Debug)]
pub struct Inst {
    pub data: HashMap<String, PyTerm>,
    pub tyinst: TyInst,
    pub var_inst: HashMap<String, PyTerm>,
    pub abs_name_inst: HashMap<String, PyTerm>,
}

#[pymethods]
impl Inst {
    #[new]
    #[pyo3(signature = (*dict, **kwargs))]
    fn new(dict: &PyTuple, kwargs: Option<&PyDict>) -> Self {
        let data: HashMap<String, PyTerm> = {
            let mut _data: HashMap<String, PyTerm> = HashMap::new();
            if !dict.is_empty() {
                let _dict = dict.get_item(0).unwrap().downcast::<PyDict>().unwrap();
                for (key, value) in _dict.iter() {
                    if let Ok(key_str) = key.extract::<String>() {
                        if let Ok(value_type) = value.extract::<PyTerm>() {
                            _data.insert(key_str, value_type);
                        }
                    }
                }
            } else if let Some(kwargs_dict) = kwargs {
                for (key, value) in kwargs_dict.iter() {
                    if let Ok(key_str) = key.extract::<String>() {
                        if let Ok(value_type) = value.extract::<PyTerm>() {
                            _data.insert(key_str, value_type);
                        }
                    }
                }
            } else {
                panic!("Invalid argument")
            }
            _data
        };

        Self {
            data: data,
            tyinst: TyInst::default(),
            var_inst: HashMap::new(),
            abs_name_inst: HashMap::new(),
        }
    }

    fn __str__(&self) -> String {
        let mut res = String::new();

        if !self.tyinst.data.is_empty() {
            res = format!("{}, ", self.tyinst.__str__());
        }

        res.push_str(
            self.data
                .iter()
                .map(|(nm, t)| format!("?{} := {}", nm, t.ptr))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );
        res.push_str(
            self.var_inst
                .iter()
                .map(|(nm, t)| format!("{} := {}", nm, t.ptr))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );
        res.push_str(
            self.abs_name_inst
                .iter()
                .map(|(nm, t)| format!("{} -> {}, ", nm, t.ptr))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );

        res
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> bool {
        match op {
            CompareOp::Eq => self.var_inst == other.var_inst,
            CompareOp::Ne => self.var_inst != other.var_inst,
            _ => self.var_inst == other.var_inst,
        }
    }
}

#[pyclass(unsendable, name = "Term", subclass)]
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PyTerm {
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

    #[getter(name)]
    fn get_name(&self) -> PyResult<String> {
        match self.ptr.as_ref() {
            Term::Var(ref name, _) => Ok(name.clone()),
            Term::Const(ref name, _) => Ok(name.clone()),
            Term::SVar(ref name, _) => Ok(name.clone()),
            _ => Err(PyAttributeError::new_err("Term has no attribute 'name'")),
        }
    }

    #[getter(T)]
    fn get_T(&self) -> PyResult<PyType> {
        match self.ptr.as_ref() {
            Term::SVar(_, ty) | Term::Var(_, ty) | Term::Const(_, ty) => {
                Ok(PyType { ptr: Rc::clone(ty) })
            }
            _ => Err(PyAttributeError::new_err("Term has no attribute 'T'")),
        }
    }

    /// consider implementing __hash__ when implementing __richcmp__
    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.ptr.hash(&mut hasher);
        hasher.finish()
    }

    /// There are some magic methods.
    fn __str__(&self) -> String {
        format!("{}", self.ptr)
    }

    fn __repr__(&self) -> String {
        self.ptr.repr()
    }

    /// Apply self (as a function) to a list of arguments.
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

    fn is_var(&self) -> bool {
        self.ptr.is_var()
    }

    fn is_svar(&self) -> bool {
        self.ptr.is_svar()
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
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn is_open(&self) -> bool {
        self.ptr.is_open()
    }

    fn subst_type(&self, tyinst: &TyInst) -> PyTerm {
        let subst_tyinst: HashMap<String, Rc<Type>> = tyinst
            .data
            .iter()
            .map(|(k, v)| (k.clone(), v.ptr.clone()))
            .collect();

        PyTerm {
            ptr: self.ptr.subst_type(
                &subst_tyinst,
                &(push_term_to_cache as fn(Term) -> ast::rc::Rc<Term>),
                &(push_type_to_cache as fn(Type) -> ast::rc::Rc<Type>),
            ),
        }
    }

    fn subst(&self, inst: &Inst) -> PyResult<PyTerm> {
        let subst_inst: HashMap<String, Rc<Term>> = inst
            .data
            .iter()
            .map(|(k, v)| (k.clone(), v.ptr.clone()))
            .collect();
        let subst_tyinst: HashMap<String, Rc<Type>> = inst
            .tyinst
            .data
            .iter()
            .map(|(k, v)| (k.clone(), v.ptr.clone()))
            .collect();
        let subst_var_inst: HashMap<String, Rc<Term>> = inst
            .var_inst
            .iter()
            .map(|(k, v)| (k.clone(), v.ptr.clone()))
            .collect();
        let subst_abs_name_inst: HashMap<String, Rc<Term>> = inst
            .abs_name_inst
            .iter()
            .map(|(k, v)| (k.clone(), v.ptr.clone()))
            .collect();

        let result = self.ptr.subst(
            subst_inst,
            subst_tyinst,
            subst_var_inst,
            subst_abs_name_inst,
            &push_type_to_cache,
            &push_term_to_cache,
        );
        match result {
            Ok(res) => Ok(PyTerm { ptr: res }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn strip_comb(&self) -> (PyTerm, Vec<PyTerm>) {
        let res = self.ptr.strip_comb();
        (
            PyTerm { ptr: res.0 },
            res.1.into_iter().map(|t| PyTerm { ptr: t }).collect(),
        )
    }

    #[pyo3(signature = (*, num=None))]
    fn strip_forall(&self, num: Option<i32>) -> (Vec<PyTerm>, PyTerm) {
        let res = self.ptr.strip_forall(num, &push_term_to_cache);
        (
            res.0.into_iter().map(|t| PyTerm { ptr: t }).collect(),
            PyTerm { ptr: res.1 },
        )
    }

    #[pyo3(signature = (*, num=None))]
    fn strip_exists(&self, num: Option<i32>) -> (Vec<PyTerm>, PyTerm) {
        let res = self.ptr.strip_exists(num, &push_term_to_cache);
        (
            res.0.into_iter().map(|t| PyTerm { ptr: t }).collect(),
            PyTerm { ptr: res.1 },
        )
    }

    fn strip_quant(&self) -> (Vec<PyTerm>, PyTerm) {
        let res = self.ptr.strip_quant(&push_term_to_cache);
        (
            res.0.into_iter().map(|t| PyTerm { ptr: t }).collect(),
            PyTerm { ptr: res.1 },
        )
    }

    #[getter(head)]
    fn get_head(&self) -> PyTerm {
        PyTerm {
            ptr: Rc::clone(&self.ptr.get_head()),
        }
    }

    #[getter(args)]
    fn get_args(&self) -> Vec<PyTerm> {
        self.ptr
            .get_args()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn is_binop(&self) -> bool {
        self.ptr.is_binop()
    }

    #[getter(arg1)]
    fn get_arg1(&self) -> PyResult<PyTerm> {
        match self.ptr.get_arg1() {
            Some(res) => Ok(PyTerm {
                ptr: Rc::clone(res),
            }),
            None => Err(PyAttributeError::new_err("Term has no attribute 'T'")),
        }
    }

    fn is_not(&self) -> bool {
        self.ptr.is_not()
    }

    fn is_implies(&self) -> bool {
        self.ptr.is_implies()
    }

    fn strip_implies(&self) -> (Vec<PyTerm>, PyTerm) {
        let res = self.ptr.strip_implies();
        (
            res.0.into_iter().map(|t| PyTerm { ptr: t }).collect(),
            PyTerm { ptr: res.1 },
        )
    }

    fn is_conj(&self) -> bool {
        self.ptr.is_conj()
    }

    fn strip_conj(&self) -> Vec<PyTerm> {
        self.ptr
            .strip_conj()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn is_disj(&self) -> bool {
        self.ptr.is_disj()
    }

    fn strip_disj(&self) -> Vec<PyTerm> {
        self.ptr
            .strip_disj()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
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

    fn is_equals(&self) -> bool {
        self.ptr.is_equals()
    }

    fn is_compares(&self) -> bool {
        self.ptr.is_compares()
    }

    fn is_reflexive(&self) -> bool {
        self.ptr.is_reflexive()
    }

    #[allow(non_snake_case)]
    fn is_VAR(&self) -> bool {
        self.ptr.is_VAR()
    }

    fn subst_bound(&self, t: &PyTerm) -> PyResult<PyTerm> {
        match self.ptr.subst_bound(&t.ptr, &push_term_to_cache) {
            Ok(res) => Ok(PyTerm { ptr: res }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn beta_conv(&self) -> PyResult<PyTerm> {
        match self.ptr.beta_conv(&push_term_to_cache) {
            Ok(res) => Ok(PyTerm { ptr: res }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn beta_norm(&self) -> PyResult<PyTerm> {
        match self.ptr.beta_norm(&push_term_to_cache) {
            Ok(res) => Ok(PyTerm { ptr: res }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn occurs_var(&self, var: &PyTerm) -> bool {
        self.ptr.occurs_var(&var.ptr)
    }

    fn abstract_over(&self, t: &PyTerm) -> PyResult<PyTerm> {
        match self.ptr.abstract_over(&t.ptr, &push_term_to_cache) {
            Ok(res) => Ok(PyTerm { ptr: res }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn checked_get_type(&self) -> PyResult<PyType> {
        match self.ptr.checked_get_type(&push_type_to_cache) {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn convert_svar(&self) -> PyResult<PyTerm> {
        match self
            .ptr
            .convert_svar(&push_term_to_cache, &push_type_to_cache)
        {
            Ok(res) => Ok(PyTerm { ptr: res }),
            Err(error) => Err(HolPyErr::from(error).into()),
        }
    }

    fn is_less_eq(&self) -> bool {
        self.ptr.is_less_eq()
    }

    fn is_less(&self) -> bool {
        self.ptr.is_less()
    }

    fn is_greater_eq(&self) -> bool {
        self.ptr.is_greater_eq()
    }

    fn is_greater(&self) -> bool {
        self.ptr.is_greater()
    }

    fn get_svars(&self) -> Vec<PyTerm> {
        self.ptr
            .get_svars()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn get_vars(&self) -> Vec<PyTerm> {
        self.ptr
            .get_vars()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn get_consts(&self) -> Vec<PyTerm> {
        self.ptr
            .get_consts()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn has_var(&self) -> bool {
        self.ptr.has_var()
    }

    fn has_vars(&self, vs: Vec<PyTerm>) -> bool {
        let vs: Vec<Rc<Term>> = vs.iter().map(|v| Rc::clone(&v.ptr)).collect();
        self.ptr.has_vars(&vs)
    }

    fn get_stvars(&self) -> Vec<PyType> {
        self.ptr
            .get_stvars()
            .into_iter()
            .map(|t| PyType { ptr: t })
            .collect()
    }

    // another way to implement
    fn get_stvars_with_pylist(&self, py: Python) -> PyObject {
        let iter = self.ptr.get_stvars().into_iter().map(|t| PyType { ptr: t });
        PyList::new(py, iter).to_object(py)
    }
}

#[pyfunction]
pub fn SVar(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_svar(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction]
pub fn Var(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_var(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction]
pub fn Const(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_const(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

// todo, Rc::clone()?
#[pyfunction]
pub fn Comb(fun: PyTerm, arg: PyTerm) -> PyTerm {
    let term = Term::new_comb(fun.ptr, arg.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction(signature = (*args))]
pub fn Abs(args: &PyTuple) -> PyTerm {
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
pub fn Bound(n: usize) -> PyTerm {
    let term = Term::new_bound(n);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

pub fn register_term_module(py: Python<'_>) -> PyResult<&PyModule> {
    let res = PyModule::new(py, "term")?;
    res.add_class::<PyTerm>()?;
    res.add_function(wrap_pyfunction!(SVar, res)?)?;
    res.add_function(wrap_pyfunction!(Var, res)?)?;
    res.add_function(wrap_pyfunction!(Const, res)?)?;
    res.add_function(wrap_pyfunction!(Comb, res)?)?;
    res.add_function(wrap_pyfunction!(Abs, res)?)?;
    res.add_function(wrap_pyfunction!(Bound, res)?)?;
    Ok(res)
}
